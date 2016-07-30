{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, DoAndIfThenElse, ScopedTypeVariables #-}
module Control.StartStop.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

import System.IO.Unsafe

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
type Sample t = ReaderT Time IO

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
newtype Hold t a = Hold { unHold :: WriterT (Pushes t) (Sample t) a } deriving (Functor, Applicative, Monad, MonadFix)
{- Like Hold but may also produce IO effects. No longer garrentes that reruns return the same vale -}
newtype HoldIO t a = HoldIO { unHoldIO :: WriterT (Pushes t) (Sample t) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

runPushes :: Hold t a -> Sample t (a, Pushes t)
runPushes (Hold h) = runWriterT h

runPushesIO :: HoldIO t a -> Sample t (a, Pushes t)
runPushesIO (HoldIO h) = runWriterT h

data EvInfo t a = NotFired | FiredNow a (Pushes t)

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
  curTime <- ask
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

data EvStream t a = Never | EvStream (Sample t (EvInfo t a))

instance Functor (EvStream t) where
  fmap _ Never = Never
  fmap f (EvStream evs) = memoEvs $ EvStream $ fmap (fmap f) evs

unsafeHoldIOSequence :: EvStream t (HoldIO t a) -> IO (EvStream t a)
unsafeHoldIOSequence Never = return Never
unsafeHoldIOSequence (EvStream emio) = do
  ref <- liftIO $ newIORef Nothing
  let unsafeEvs = do
        curTime <- ask
        mOld <- liftIO $ readIORef ref
        case mOld of
          Just (t, v, p)
            | t == curTime -> return $ FiredNow v p
          _ -> do
            mio <- emio
            case mio of
              NotFired -> return NotFired
              FiredNow hiov p -> do
                (v, pv) <- runPushesIO hiov
                t <- ask
                liftIO $ writeIORef ref (Just (t, v, p <> pv))
                return $ FiredNow v p

  return $ EvStream unsafeEvs

unsafeIOSequence :: EvStream t (IO a) -> IO (EvStream t a)
unsafeIOSequence = unsafeHoldIOSequence . fmap liftIO

listenPushes :: EvStream t a -> EvStream t (a, Pushes t)
listenPushes = pushes . fmap listen . unPushes

type VoidEvent t = Sample t (Maybe Time) -- Void events fire and then return the time right before they fired
                                         -- at all time past that time.

data BehaviorInfo t a = BehaviorInfo { currentVal :: a -- the current value of the behavior
                                     , futureInfo :: Sample t (EvInfo t (BehaviorInfo t a))
                                     -- let the current time be t, and let t' be "just after t".
                                     -- If b t /= b t', then futureInfo will return the behaviorInfo
                                     -- for the behavior b at t'. This allows the ability to be able to
                                     -- as if behaviors immediatly started holding there value. It also
                                     -- helps with memoization.
                                     , changeTime :: VoidEvent t
                                     -- a void event that fires when the current value is no longer
                                     -- the current value. This is used to greatly improve the power
                                     -- of memoization
                                     }

firstTime :: VoidEvent t -> VoidEvent t -> VoidEvent t
firstTime smt1 smt2 = do
  mt1 <- smt1
  mt2 <- smt2
  case (mt1, mt2) of
    (Nothing, Nothing) -> return Nothing
    (Just lt, Nothing) -> return $ Just lt
    (Nothing, Just rt) -> return $ Just rt
    (Just lt, Just rt)
      | lt <= rt -> return $ Just lt
      | rt < lt -> return $ Just rt

data Behavior t a = BConst a | Behavior (Hold t (BehaviorInfo t a))

runB :: Behavior t a -> Hold t (BehaviorInfo t a)
runB (BConst x) = return $ BehaviorInfo x (return NotFired) (return Nothing)
runB (Behavior hbi) = hbi

{-# NOINLINE memoB #-}
memoB :: Behavior t a -> Behavior t a
memoB (BConst x) = BConst x
memoB b = unsafePerformIO $ do
  ref <- newIORef Nothing -- stores the value of right now
  futureRef <- newIORef Nothing -- stores the value of the upper limit of now

  return $ usePrevB ref futureRef b

usePrevB :: forall t a . IORef (Maybe (Time, BehaviorInfo t a, Pushes t)) -- currentValRef
                         -> IORef (Maybe (Time, BehaviorInfo t a, Pushes t)) -- futureRef
                         -> Behavior t a -- b
                         -> Behavior t a
usePrevB currentValRef futureRef bToMemo = Behavior $ Hold $ do
    let reRun = do
          {- samples bToMemo and stores the value, for use latter on.
          - This calculation may take more time than just looking up something
          - in an IORef, so this should be run as few times a possible.
          -}
          curTime <- ask
          (BehaviorInfo v sFuture sct, p) <- listen . unHold $ runB bToMemo
          let mf = memoFuture sFuture
          liftIO $ writeIORef currentValRef (Just (curTime, BehaviorInfo v mf sct, p))
          return $ BehaviorInfo v mf sct

        {-
        - Stores the value from any sampleAfters or changes to futureRef.
        -}
        memoFuture :: Sample t (EvInfo t (BehaviorInfo t a)) -> Sample t (EvInfo t (BehaviorInfo t a))
        memoFuture sFuture = do
          curTime <- ask
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
        checkFutureRefValid = do
          curTime <- ask
          mtfuture <- liftIO $ readIORef futureRef
          case mtfuture of
            Just (tf', bInfo', p')
              | tf' /= curTime -> do
                ct' <- lift $ changeTime bInfo'
                case ct' of
                  Just ft'
                    | ft' < curTime -> reRun
                  _ -> do
                    liftIO $ writeIORef currentValRef $ Just (curTime, bInfo', p')
                    tell p'
                    return bInfo'
            _ -> reRun

    {-
    - checks to see if currentValRef if it is valid. If it is still valid, it
    - uses that as the value of the behavior. If it is not it checkFutureRefValid
    -}
    curTime <- ask
    mtv <- liftIO $ readIORef currentValRef

    case mtv of
      Nothing -> reRun
      Just (t, bInfo, p) -> do
        ct <- lift $ changeTime bInfo
        case ct of
          Just t'
            | t' < curTime -> checkFutureRefValid
          _ -> do
            tell p
            return bInfo

instance Functor (Behavior t) where
  fmap = liftM

instance Applicative (Behavior t) where
  pure = return
  (<*>) = ap

leftMostChange :: Sample t (EvInfo t a) -> Sample t (EvInfo t a) -> Sample t (EvInfo t a)
leftMostChange se1 se2 = do
  e1 <- se1

  case e1 of
    NotFired -> se2
    FiredNow a pa -> return $ FiredNow a pa

instance Monad (Behavior t) where
  return = BConst
  (BConst a) >>= f = f a
  bh >>= f = memoB $ Behavior $ do
    (BehaviorInfo a sFutureA sta) <- runB bh
    (BehaviorInfo b sFutureB stb) <- runB (f a)

    let sFutureF sFutureA = do
          futureA <- sFutureA

          case futureA of
            NotFired -> sFutureB
            FiredNow (BehaviorInfo a' sFutureA' sta') pa' -> do
              (bInfo@(BehaviorInfo b' sFutureB' stb'), pb') <- runWriterT . unHold $ runB $ f a'
              futureB' <- sFutureB'
              case futureB' of
                NotFired -> return $ FiredNow (BehaviorInfo b' (leftMostChange (sFutureF sFutureA') sFutureB') (firstTime sta' stb')) (pa' <> pb')
                FiredNow (BehaviorInfo fb' sFutureFB' stfb') pfb' -> return $ FiredNow (BehaviorInfo fb' sFutureFB' (firstTime sta' stfb')) (pa' <> pb' <> pfb')

    return $ BehaviorInfo b (sFutureF sFutureA) (firstTime sta stb)

instance MonadFix (Behavior t) where
  mfix f = Behavior $ mfix (\(~(BehaviorInfo a _ _)) -> runB $ f a)

newChangeTime :: IO (VoidEvent t, Time -> IO ())
newChangeTime = do
  ct <- newIORef Nothing

  let sct = do
        curTime <- ask
        mt <- liftIO $ readIORef ct
        case mt of
          Just t -> return $ Just t
          Nothing -> return Nothing

  return (sct, writeIORef ct . Just)

newtype Plans t = Plans (EvStream t ())

instance Monoid (Plans t) where
  mempty = Plans never
  mappend (Plans el) (Plans er) = Plans $ mergefEs (<>) el er

data Env = Env { clock :: IO Time, scheduleRound :: IO () }
newtype PlanHold t a = PlanHold { unPlanHold :: WriterT (Plans t, Pushes t) (ReaderT Env IO) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadHold PlanHold t where
  liftHold (Hold h) = PlanHold $ do
    iot <- asks clock
    t <- liftIO iot
    (a, p) <- liftIO $ runReaderT (runWriterT h) t
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

instance MonadHold HoldIO t where
  liftHold (Hold rw) = HoldIO rw

{- unPushes and pushes are useful for slight improvements to efficency
   allows for things leftmost to not need the pushes of the unused value.
-}
type PushOnly t = Writer (Pushes t)
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
mergefEs f (EvStream mel) (EvStream mer) = memoEvs $ EvStream $ do
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
startOnFire (EvStream me) = memoEvs $ EvStream $ do
  eInfo <- me
  case eInfo of
    NotFired -> return NotFired
    FiredNow ha pha -> do
      (a, pa) <- runPushes ha
      return $ FiredNow a (pha <> pa)

catMabyeEs :: EvStream t (Maybe a) -> EvStream t a
catMabyeEs Never = Never
catMabyeEs (EvStream me) = memoEvs $ EvStream $ do
  eInfo <- me
  case eInfo of
    NotFired -> return NotFired
    FiredNow ma pma -> case ma of
      Nothing -> return NotFired
      Just a -> return $ FiredNow a pma

coincidence :: EvStream t (EvStream t a) -> EvStream t a
coincidence Never = Never
coincidence (EvStream em) = memoEvs $ EvStream $ do
  eInfo <- em
  case eInfo of
    NotFired -> return NotFired
    FiredNow (EvStream em2) p1 -> do
      eInfo2 <- em2
      case eInfo2 of
        NotFired -> return NotFired
        FiredNow a p2 -> return $ FiredNow a (p1 <> p2)


sample :: Behavior t a -> Hold t a
sample = fmap currentVal . runB

sampleAfter :: Behavior t a -> Hold t a
sampleAfter (BConst x) = return x
sampleAfter b = Hold $ do
  (BehaviorInfo a sfa _, p) <- lift $ runWriterT . unHold $ runB b
  fa <- lift sfa
  case fa of
    NotFired -> tell p >> return a
    FiredNow aInfo' p' -> tell p' >> return (currentVal aInfo')

changes :: Behavior t a -> EvStream t a
changes (BConst _) = never
changes b = EvStream $ do
  (BehaviorInfo _ sfa _, _) <- runWriterT . unHold $ runB b
  fmap currentVal <$> sfa

holdEs :: EvStream t a -> a -> Hold t (Behavior t a)
holdEs Never iv = return (return iv)
holdEs evs iv = Hold $ do
  startTime <- ask

  (oTime, trigger) <- liftIO newChangeTime
  ref <- liftIO $ newIORef (iv, mempty, oTime, trigger)

  let pushAction t newVal subEffects sct trigger = do
        (_, _, _, oldTrigger) <- readIORef ref
        oldTrigger t
        writeIORef ref (newVal, subEffects, sct, trigger)

      subPushes = Pushes $ EvStream $ do
        (_, Pushes evEffs, _, _) <- liftIO (readIORef ref)
        case evEffs of
          Never -> return NotFired
          (EvStream effs) -> effs

  (EvStream mToPush) <- liftIO $ fmap catMabyeEs $ unsafeHoldIOSequence $ flip fmap (listenPushes evs) $ \(a, p) -> HoldIO $ do
                                t <- ask
                                if startTime == t
                                then return Nothing
                                else do
                                  (newCT, trigger) <- liftIO newChangeTime
                                  return $ Just (t, a, p, newCT, trigger)

  tell $ Pushes $ (\(t, a, p, ct, trigger) -> pushAction t a p ct trigger) <$> EvStream mToPush
  tell subPushes

  return $ Behavior $ Hold $ do
    (v, effects, sct, _) <- liftIO $ readIORef ref
    tell effects
    let sFuture = fmap (fmap (\(_,v,_,sct',_) -> BehaviorInfo v sFuture sct')) mToPush
    return $ BehaviorInfo v sFuture sct

switch :: Behavior t (EvStream t a) -> EvStream t a
switch (BConst evs) = evs
switch bevs = EvStream $ do
  (BehaviorInfo (EvStream me) _ _, _) <- runWriterT . unHold $ runB bevs
  me

{-# NOINLINE unsafeHoldIOMap #-}
unsafeHoldIOMap :: EvStream t (HoldIO t a) -> EvStream t a
unsafeHoldIOMap = unsafePerformIO . unsafeHoldIOSequence

unsafeIOMap :: EvStream t (IO a) -> EvStream t a
unsafeIOMap = unsafeHoldIOMap . fmap liftIO

unsafePlan :: EvStream t (IO a) -> Hold t (EvStream t a)
unsafePlan evs = Hold $ do
  plans <- liftIO $ unsafeIOSequence evs
  tell $ Pushes $ return () <$ plans
  return plans
