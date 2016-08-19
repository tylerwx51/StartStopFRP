{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, DoAndIfThenElse, ScopedTypeVariables #-}
module Control.StartStop.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef
import Control.Concurrent.MVar

import System.IO.Unsafe
import System.Mem.Weak

{--
-- EvStream a = Time -> Maybe a
-- Behavior a = Time -> a
--
--
--}

{--
-- Time is for internal use only. Time is ordered, and will always increase
-- after each round. Time this implementation is the round number. A round ends
-- after all simulatnious events are dealt with.
--}
newtype Time = T Integer deriving (Eq, Ord, Show)

{--
-- Pushes is used to tell the main loop when it needs to
-- run IO actions that will ensure behaviors created by holdEs is up to date.
--}
newtype Pushes t = Pushes (EvStream t (IO ()))

instance Monoid (Pushes t) where
  mempty = Pushes never
  mappend (Pushes el) (Pushes er) = Pushes $ mergefEs (>>) el er

{-
PushStream is similar to EvStream.
PushStream also returns an updated version of itself. So if at time t a stream "original" has
the value (pInfo, self), we know the following:
  Semantically, self = original
However, it can remove streams that are known to no longer fire, therefore removing
them from taking space and having to be run.
-}
data PushInfo a = PNotFired | PFired a
data PushStream t = PNever | Pls (Sample t (PushInfo (IO ()), PushStream t))

evStreamToPushStream :: EvStream t (Maybe (IO ())) -> PushStream t
evStreamToPushStream Never = PNever
evStreamToPushStream evs@(EvStream s) = Pls $ do
  eInfo <- s
  case eInfo of
    NotFired -> return (PNotFired, evStreamToPushStream evs)
    FiredNow mv _ -> case mv of
      Just v -> return (PFired v, evStreamToPushStream evs)
      Nothing -> return (PNotFired, PNever)

mergePushStream :: PushStream t -> PushStream t -> PushStream t
mergePushStream PNever p = p
mergePushStream p PNever = p
mergePushStream (Pls leftS) (Pls rightS) = Pls $ do
  (leftPushInfo, leftNext) <- leftS
  (rightPushInfo, rightNext) <- rightS
  let nextStream = mergePushStream leftNext rightNext
  case (leftPushInfo, rightPushInfo) of
    (PNotFired, PNotFired) -> return (PNotFired, nextStream)
    (PNotFired, rInfo) -> return (rInfo, nextStream)
    (lInfo, PNotFired) -> return (lInfo, nextStream)
    (PFired leftIO, PFired rightIO) -> return (PFired $ leftIO >> rightIO, nextStream)

instance Monoid (PushStream t) where
  mempty = PNever
  mappend = mergePushStream

{-
- Sample t a : is semantically a (Time -> a)
- Sample t a may perform IO however, the following statement must be true:
- given sample and t, do
    a <- runReaderT sample t
    ...
    b <- runReaderT sample t
    then a = b must always be true no matter what happend was called during the ...
- Rougly if you run the sample, it is garrented to return the same value if you call it more than once.
- The IO is specificly used for several performance reasons (memoization and in holdEs).
-}
newtype Sample t a = Sample { unSample :: ReaderT Time IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

getCurTime :: Sample t Time
getCurTime = Sample ask

{-
- Hold t a : is semantically (Time -> (a, Pushes))
- Hold is simalar to a sample, but now we also have the ability to start
- holding, the Pushes are what pushes are neccisary to make the current values
- work. Similar to sample, IO may be performed but an reOccuring runHolds must
- return the same value.
-
- TODO: I believe that it is possible to reduce the work for duplicate holds, like in
-       the following examples:
-        expamle1 : do
              holdEs ev 1
              holdEs ev 2

         example2 : do
              holdEs ev 1
              let ev2 <- pullAlways $ fmap (\_ -> holdEs ev 5) ev
              holdEs ev2 (return (negate 1))
-
-       I however, need to look into how far I can push this and how it will
-       effect performance.
-}
newtype HoldInternal t a = HoldIn { unHoldIn :: WriterT (PushStream t) (Sample t) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

liftSample :: Sample t a -> HoldInternal t a
liftSample = HoldIn . lift

newtype Hold t a = Hold (HoldInternal t a) deriving (Functor, Applicative, Monad, MonadFix)

getCurrentTime :: HoldInternal t Time
getCurrentTime = HoldIn $ lift getCurTime

addPushStream :: PushStream t -> HoldInternal t ()
addPushStream = HoldIn . tell

unHold :: Hold t a -> WriterT (PushStream t) (Sample t) a
unHold (Hold hi) = unHoldIn hi

{- Like Hold but may also produce IO effects. No longer garrentes that reruns return the same vale -}
newtype HoldIO t a = HoldIO { unHoldIO :: WriterT (PushStream t) (Sample t) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

runPushes :: Hold t a -> Sample t (a, PushStream t)
runPushes h = runWriterT $ unHold h

runPushesIO :: HoldIO t a -> Sample t (a, PushStream t)
runPushesIO (HoldIO h) = runWriterT h

data EvInfo t a = NotFired | FiredNow !a !(PushStream t)

instance Functor (EvInfo t) where
  fmap = liftM

instance Applicative (EvInfo t) where
  pure = return
  (<*>) = ap

instance Monad (EvInfo t) where
  return x = FiredNow x mempty
  NotFired >>= _ = NotFired
  (FiredNow a pushesA) >>= f = case f a of
    NotFired -> NotFired
    FiredNow b pushesB -> FiredNow b (pushesA <> pushesB)

swap :: EvInfo t (Hold t a) -> Hold t (EvInfo t a)
swap NotFired = return NotFired
swap (FiredNow ha pushes) = do
  a <- ha
  return $ FiredNow a pushes

{-# NOINLINE memoSample #-}
memoSample :: Sample t a -> Sample t a
memoSample s = unsafePerformIO $ do
  currentValRef <- newIORef Nothing
  return $ usePrevSample currentValRef s

{-
- Since samples are guaranteed to return the same value given a time t
- we can store the value of the sample at a given time. The IORef will
- store the most recent value when the sample was run, and when it was run.
- If the IORef holds a value that was run during the same round, then it
- uses that value instead of recalcuating.
--}
usePrevSample :: IORef (Maybe (Time, a)) -> Sample t a -> Sample t a
usePrevSample currentValRef recalcSample = do
  mvt <- liftIO $ readIORef currentValRef
  curTime <- getCurTime
  case mvt of
    Just (t, v)
      | t == curTime -> return v
    _ -> do
      a <- recalcSample
      liftIO $ writeIORef currentValRef (Just (curTime, a))
      return a

memoEvs :: EvStream t a -> EvStream t a
memoEvs Never = Never
memoEvs (EvStream evs) = EvStream $ memoSample evs

{-
- TODO : Allow for simultanious events to be stored efficiently, basicly have a list of events instead of
-        on/off switch.
-
-}
data EvStream t a = Never | EvStream (Sample t (EvInfo t a))

instance Functor (EvStream t) where
  fmap _ Never = Never
  fmap f (EvStream evs) = memoEvs $ EvStream $ fmap (fmap f) evs

unsafeHoldIOSequence :: EvStream t (HoldIO t a) -> IO (EvStream t a)
unsafeHoldIOSequence evs = do
  ref <- liftIO $ newMVar Nothing
  let unsafeEvs emio = EvStream $ do
        curTime <- getCurTime
        mOld <- liftIO $ takeMVar ref
        case mOld of
          Just (t, v, p)
            | t == curTime -> do
              liftIO $ putMVar ref mOld
              return $ FiredNow v p
            | t > curTime -> error "Invariant Broken."
          _ -> do
            mio <- emio
            case mio of
              NotFired -> do
                liftIO $ putMVar ref Nothing
                return NotFired
              FiredNow hiov p -> do
                (v, pv) <- runPushesIO hiov
                t <- getCurTime
                liftIO $ putMVar ref (Just (t, v, p <> pv))
                return $ FiredNow v p

  return $ case evs of
            Never -> Never
            EvStream emio -> unsafeEvs emio

unsafeIOSequence :: EvStream t (IO a) -> IO (EvStream t a)
unsafeIOSequence = unsafeHoldIOSequence . fmap liftIO

listenPushes :: EvStream t a -> EvStream t (a, PushStream t)
listenPushes Never = Never
listenPushes (EvStream sEInfo) = EvStream $ do
  eInfo <- sEInfo
  case eInfo of
    NotFired -> return NotFired
    FiredNow a p -> return $ FiredNow (a, p) p

data BehaviorInfo t a = BehaviorInfo { currentVal :: !a -- the current value of the behavior
                                     , futureInfo :: Sample t (EvInfo t (BehaviorInfo t a))
                                     -- let the current time be t, and let t' be "just after t".
                                     -- If b t /= b t', then futureInfo will return the behaviorInfo
                                     -- for the behavior b at t'. This allows the ability to be able to
                                     -- as if behaviors immediatly started holding there value. It also
                                     -- helps with memoization.
                                     , lastChangeTime :: Sample t (Maybe Time)
                                     -- lastChangeTime returns the value of when the last change to
                                     -- the behavior occured. It will be updated at the same time the value changes
                                     }

mostRecent :: Sample t (Maybe Time) -> Sample t (Maybe Time) -> Sample t (Maybe Time)
mostRecent smt1 smt2 = maxOfJust <$> smt1 <*> smt2
  where
    maxOfJust Nothing (Just tl) = Just tl
    maxOfJust (Just tr) Nothing = Just tr
    maxOfJust (Just tl) (Just tr) = Just (max tl tr)
    maxOfJust Nothing Nothing = Nothing

data Behavior t a = Behavior { runB :: HoldInternal t (BehaviorInfo t a) }

{-# NOINLINE memoB #-}
memoB :: Behavior t a -> Behavior t a
memoB b = unsafePerformIO $ do
  ref <- newIORef Nothing -- stores the value of right now
  futureRef <- newIORef Nothing -- stores the value of the upper limit of now

  return $ usePrevB ref futureRef b

usePrevB :: forall t a . IORef (Maybe (Time, BehaviorInfo t a, PushStream t)) -- currentValRef
                         -> IORef (Maybe (Time, BehaviorInfo t a, PushStream t)) -- futureRef
                         -> Behavior t a -- b
                         -> Behavior t a
usePrevB currentValRef futureRef bToMemo = Behavior $ do
    let reRun :: HoldInternal t (BehaviorInfo t a)
        reRun = do
          {- samples bToMemo and stores the value, for use latter on.
          - This calculation may take more time than just looking up something
          - in an IORef, so this should be run as few times a possible.
          -}
          curTime <- getCurrentTime
          (BehaviorInfo v sFuture sct, p) <- HoldIn $ listen . unHoldIn $ runB bToMemo
          let mf = memoFuture sFuture
          liftIO $ writeIORef currentValRef (Just (curTime, BehaviorInfo v mf sct, p))
          return $ BehaviorInfo v mf sct

        {-
        - Stores the value from any sampleAfters or changes to futureRef.
        -}
        memoFuture :: Sample t (EvInfo t (BehaviorInfo t a)) -> Sample t (EvInfo t (BehaviorInfo t a))
        memoFuture sFuture = do
          curTime <- getCurTime
          mtv <- liftIO $ readIORef futureRef
          case mtv of
            Just (t, bInfo, p)
              | t == curTime -> return $ FiredNow bInfo p
            _ -> do
              evFuture <- sFuture
              case evFuture of
                NotFired -> return NotFired
                FiredNow bInfo' p' -> do
                  liftIO $ writeIORef futureRef $ Just (curTime, bInfo', p')
                  return $ FiredNow bInfo' p'

        {- checkFutureRefValid is run if the value in currentValRef is no longer
        - valid. This then checks to see if the value in futureRef is still valid.
        - If it is it makes the futureRef value into the currentValRef. This means
        - we will not recalculate the current value if it was original found
        - by a sampleAfter or changes first. If futureRef is not valid it recalculates
        - the value for the behavior
        -}
        checkFutureRefValid :: HoldInternal t (BehaviorInfo t a)
        checkFutureRefValid = do
          curTime <- getCurrentTime
          mtfuture <- liftIO $ readIORef futureRef

          case mtfuture of
            Just (tf', bInfo', p') -> do
                mct' <- liftSample $ lastChangeTime bInfo'
                case mct' of
                  Just ct'
                    | tf' == ct' -> do -- Since futureRef can only be grabbed on the round it changes,
                      liftIO $ writeIORef currentValRef $ Just (curTime, bInfo', p')
                      liftIO $ writeIORef futureRef Nothing
                      addPushStream p'
                      return bInfo'
                  _ -> reRun
            _ -> reRun

    {-
    - checks to see if currentValRef if it is valid. If it is still valid, it
    - uses that as the value of the behavior. If it is not it checkFutureRefValid
    -}
    mtv <- liftIO $ readIORef currentValRef

    case mtv of
      Nothing -> reRun
      Just (t, bInfo, p) -> do
        mct <- liftSample $ lastChangeTime bInfo
        case mct of
          Just ct
            | ct < t -> do
              addPushStream p
              return bInfo
          _ -> checkFutureRefValid

instance Functor (Behavior t) where
  fmap = liftM

instance Applicative (Behavior t) where
  pure = return
  (<*>) = ap

instance Monad (Behavior t) where
  return x = Behavior $ return $ BehaviorInfo x (return NotFired) (return Nothing)
  bh >>= f = memoB $ Behavior $ do
    (BehaviorInfo a sFutureA sta) <- runB bh
    (BehaviorInfo b sFutureB stb) <- runB (f a)

    let sFutureF sFutureA sFutureB = do
          futureA <- sFutureA

          case futureA of
            NotFired -> sFutureB
            FiredNow (BehaviorInfo a' sFutureA' sta') pa' -> do
              (bInfo@(BehaviorInfo b' sFutureB' stb'), pb') <- runWriterT . unHoldIn $ runB $ f a'
              futureB' <- sFutureB'
              case futureB' of
                NotFired -> return $ FiredNow (BehaviorInfo b' (sFutureF sFutureA' sFutureB') (mostRecent sta' stb')) (pa' <> pb')
                FiredNow (BehaviorInfo fb' sFutureFB' stfb') pfb' -> return $ FiredNow (BehaviorInfo fb' sFutureFB' (mostRecent sta' (mostRecent stb' stfb'))) (pa' <> pb' <> pfb')

    return $ BehaviorInfo b (sFutureF sFutureA sFutureB) (mostRecent sta stb)

instance MonadFix (Behavior t) where
  mfix f = Behavior $ mfix (\(~(BehaviorInfo a _ _)) -> runB $ f a)

newtype Plans t = Plans (EvStream t ())

instance Monoid (Plans t) where
  mempty = Plans never
  mappend (Plans el) (Plans er) = Plans $ mergefEs (<>) el er

data Env = Env { clock :: IO Time, scheduleRound :: IO () }
newtype PlanHold t a = PlanHold { unPlanHold :: WriterT (Plans t, PushStream t) (ReaderT Env IO) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadHold PlanHold t where
  liftHold h = PlanHold $ do
    iot <- asks clock
    t <- liftIO iot
    (a, p) <- liftIO $ runReaderT (unSample $ runWriterT $ unHold h) t
    tell (mempty, p)
    return a

planEs :: EvStream t (IO a) -> PlanHold t (EvStream t a)
planEs Never = return Never
planEs evs = PlanHold $ do
  plans <- liftIO $ unsafeIOSequence evs

  tell (Plans $ void plans, mempty)
  return plans


-- Interface --

{- lifting hold, similar to liftIO. -}
class (Monad (m t)) => MonadHold m t where
  liftHold :: Hold t a -> m t a
  --sample :: m t a

instance MonadHold Hold t where
  liftHold = id

instance MonadHold HoldIO t where
  liftHold h = HoldIO $ unHold h

{- unPushes and pushes are useful for slight improvements to efficency
   allows for things leftmost to not need the pushes of the unused value.
-}
type PushOnly t = Writer (PushStream t)
unPushes :: EvStream t a -> EvStream t (PushOnly t a)
unPushes Never = Never
unPushes (EvStream em) = EvStream $ do
  eInfo <- em
  case eInfo of
    NotFired -> return NotFired
    FiredNow a p -> return $ FiredNow (tell p >> return a) mempty

pushes :: EvStream t (PushOnly t a) -> EvStream t a
pushes Never = Never
pushes (EvStream em) = EvStream $ do
  eInfo <- em
  case eInfo of
    NotFired -> return NotFired
    FiredNow w p2 -> do
      let (v, p1) = runWriter w
      return $ FiredNow v (p1 <> p2)

{--
-- never is an EvStream that never fires.
--}
never :: EvStream t a
never = Never

{--
-- "mergefEs f es1 es2" is an EvStream that fires when either e1 or e2
-- fires. If e1 and e2 fire at the same time, let v1 be the value of the event
-- fired by e1, and v2 be the value of the event fired by e2. Then "mergefEs f es1 es2"
-- fires with a value of "f v1 v2"
--}
mergefEs :: (a -> a -> a) -> EvStream t a -> EvStream t a -> EvStream t a
mergefEs _ Never e = e
mergefEs _ e Never = e
mergefEs f (EvStream mel) (EvStream mer) = EvStream $ do
  el <- mel
  er <- mer
  case (el, er) of
    (NotFired, NotFired) -> return NotFired
    (FiredNow l p, NotFired) -> return $ FiredNow l p
    (NotFired, FiredNow r p) -> return $ FiredNow r p
    (FiredNow _ _, FiredNow _ _) -> return $ liftA2 f el er

{--
-- "startOnFire ev" when an event fires the hold will be started.
--}
startOnFire :: EvStream t (Hold t a) -> EvStream t a
startOnFire Never = Never
startOnFire (EvStream me) = EvStream $ do
  eInfo <- me
  case eInfo of
    NotFired -> return NotFired
    FiredNow ha pha -> do
      (a, pa) <- runPushes ha
      return $ FiredNow a (pha <> pa)

{-
- an EvStream that fires when ever the sorce fires with the value of Just a.
- The EvStream fires with a value of a.
-}
catMaybeEs :: EvStream t (Maybe a) -> EvStream t a
catMaybeEs Never = Never
catMaybeEs (EvStream me) = EvStream $ do
  eInfo <- me
  case eInfo of
    NotFired -> return NotFired
    FiredNow ma pma -> case ma of
      Nothing -> return NotFired
      Just a -> return $ FiredNow a pma

{-
- Fires whenever both the outer and inner event streams fire at the same time.
-}
coincidence :: EvStream t (EvStream t a) -> EvStream t a
coincidence Never = Never
coincidence (EvStream em) = EvStream $ do
  eInfo <- em
  case eInfo of
    NotFired -> return NotFired
    FiredNow (EvStream em2) p1 -> do
      eInfo2 <- em2
      case eInfo2 of
        NotFired -> return NotFired
        FiredNow a p2 -> return $ FiredNow a (p1 <> p2)

{- Sample the behavior in a hold at the current time. -}
sample :: Behavior t a -> Hold t a
sample = Hold . fmap currentVal . runB

{- give the value of the behavior "just after" the current time. -}
sampleAfter :: Behavior t a -> Hold t a
sampleAfter b = Hold $ do
  (BehaviorInfo a sfa _, p) <- liftSample $ runWriterT . unHoldIn $ runB b
  fa <- liftSample sfa
  case fa of
    NotFired -> addPushStream p >> return a
    FiredNow aInfo' p' -> addPushStream p' >> return (currentVal aInfo')

{- Whenever the behavior changes value, the produced event stream fires-}
changes :: Behavior t a -> EvStream t a
changes b = EvStream $ do
  (BehaviorInfo _ sfa _, _) <- runWriterT . unHoldIn $ runB b
  fmap currentVal <$> sfa

{- TODO : the result of a foldEs does not ever get destroyed currently, I need to work on this. -}
{- Creates a behavior whos value is the most recent (not currently occuring) event's value. -}
holdEs :: EvStream t a -> a -> Hold t (Behavior t a)
holdEs evs iv = Hold $ do
  startTime <- getCurrentTime
  ref <- liftIO $ newIORef (iv, mempty, Nothing)

  {- The weak refrence is used to write to, if nobody can read from ref
  -- (aka. nobody samples the end behavior), then their is no reason to
  -- write to it. Since it is a weak refence it will not write anything
  -- to the refrence if it no longer is in use.
  -}
  weakRef <- liftIO $ mkWeakIORef ref (return ())

  let subPushes prevT prevPush = Pls $ do
        mr <- liftIO $ deRefWeak weakRef
        case mr of
          Nothing -> return (PNotFired, PNever)
          Just r -> do
            (_, pStream, mt) <- liftIO (readIORef r)
            let currentPush = if mt == prevT then prevPush else pStream
            case currentPush of
              PNever -> return (PNotFired, subPushes prevT PNever)
              (Pls sEff) -> do
                (eff, next) <- sEff
                return (eff, subPushes prevT next)

      primaryPushes Never = PNever
      primaryPushes evs@(EvStream sEInfo) = Pls $ do
        mr <- liftIO $ deRefWeak weakRef
        case mr of
          Just r -> do
            eInfo <- sEInfo
            case eInfo of
              NotFired -> return (PNotFired, primaryPushes evs)
              FiredNow (t, newVal, subEffects) _ -> do
                let pushAction = writeIORef r (newVal, subEffects, Just t)
                return (PFired pushAction, primaryPushes evs)
          Nothing -> return (PNotFired, PNever)

      toPush = catMaybeEs $ startOnFire $ flip fmap (listenPushes evs) $ \(a, p) -> Hold $ do
                                t <- getCurrentTime
                                if startTime == t
                                then return Nothing
                                else return $ Just (t, a, p)

  addPushStream $ primaryPushes toPush
  addPushStream $ subPushes Nothing PNever

  return $ Behavior $ do
    (v, effects, _) <- liftIO $ readIORef ref
    addPushStream effects
    let lastChange = liftIO $ (\(_,_,sct) -> sct) <$> readIORef ref
        sFuture = case toPush of
                    Never -> return NotFired
                    EvStream mToPush -> fmap (fmap (\(_,v,_) -> BehaviorInfo v sFuture lastChange)) mToPush
    return $ BehaviorInfo v sFuture lastChange

{- Creates a event stream that fires whenever the current event stream fires. -}
switch :: Behavior t (EvStream t a) -> EvStream t a
switch bevs = EvStream $ do
  (BehaviorInfo me _ _, _) <- runWriterT . unHoldIn $ runB bevs
  case me of
    Never -> return NotFired
    EvStream em' -> em'

{-# NOINLINE unsafeHoldIOMap #-}
unsafeHoldIOMap :: EvStream t (HoldIO t a) -> EvStream t a
unsafeHoldIOMap Never = Never
unsafeHoldIOMap evs = unsafePerformIO . unsafeHoldIOSequence $ evs

unsafeIOMap :: EvStream t (IO a) -> EvStream t a
unsafeIOMap = unsafeHoldIOMap . fmap liftIO

unsafePlan :: EvStream t (IO a) -> Hold t (EvStream t a)
unsafePlan evs = Hold $ do
  plans <- liftIO $ unsafeIOSequence evs
  addPushStream $ evStreamToPushStream $ Just (return ()) <$ plans
  return plans
