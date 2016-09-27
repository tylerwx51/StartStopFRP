{-# LANGUAGE GeneralizedNewtypeDeriving, DoAndIfThenElse, ScopedTypeVariables, DeriveFunctor #-}

module Control.StartStop.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef
import Control.Concurrent.MVar

import System.IO.Unsafe
import System.Mem.Weak

newtype Time = T Integer deriving (Eq, Ord, Show)

newtype Sample t a = Sample { unSample :: ReaderT Time IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

{-
PushStream is similar to EvStream.
PushStream also returns an updated version of itself. So if at time t a stream "original" has
the value (pInfo, self), we know the following:
  Semantically, self = original
However, it can remove streams that are known to no longer fire, therefore removing
them from taking space and having to be run.
-}
data PushInfo a = PNotFired | PFired !a
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

getCurTime :: Sample t Time
getCurTime = Sample ask

data EvInfo t a = NotFired
                | FiredNow !a !(PushStream t) deriving(Functor)

data EvStream t a = Never
                  | EvStream (Sample t (EvInfo t a))

type BehaviorInternal t a = WriterT (PushStream t) (Sample t) a
newtype Behavior t a = Behavior { runB :: BehaviorInternal t a } deriving (Functor, Applicative, Monad, MonadFix)

data ReactiveInfo t a = ReactiveInfo { currentValue :: !a
                                     , changesInfo :: EvStream t a
                                     , lastChangeTime :: Sample t (Maybe Time)
                                     , sideEffects :: PushStream t
                                     }
data Reactive t a = Reactive { sampleReactive :: Sample t (ReactiveInfo t a) }

sampleEvStream :: EvStream t a -> Sample t (EvInfo t a)
sampleEvStream Never = return NotFired
sampleEvStream (EvStream sEInfo) = sEInfo

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
memoEvs (EvStream sampleEvs) = EvStream $ memoSample $ memoSample sampleEvs

{-# NOINLINE memoReactive #-}
memoReactive :: Reactive t a -> Reactive t a
memoReactive r = unsafePerformIO $ do
  currentRef <- newIORef Nothing
  futureRef <- newIORef Nothing
  return $ usePrevR currentRef futureRef r

usePrevR :: forall t a . IORef (Maybe (Maybe Time, a, PushStream t))
         -> IORef (Maybe (Maybe Time, a, PushStream t))
         -> Reactive t a
         -> Reactive t a
usePrevR currentRef futureRef r = Reactive $ do
  rInfo <- sampleReactive r
  let mostRecentChange = lastChangeTime rInfo
      changes = EvStream $ do
        mtv' <- liftIO $ readIORef futureRef
        curTime <- getCurTime
        case mtv' of
          Just (t', v', p')
            | t' == Just curTime -> return $ FiredNow v' p'
          _ -> do
            changeInfo <- sampleEvStream $ changesInfo rInfo
            case changeInfo of
              NotFired -> return NotFired
              FiredNow v' p' -> do
                liftIO $ writeIORef futureRef $ Just (Just curTime, v', p')
                return $ FiredNow v' p'

      reRun :: Sample t (ReactiveInfo t a)
      reRun = do
        curTime <- getCurTime
        (ReactiveInfo v _ _ p) <- sampleReactive r
        liftIO $ writeIORef currentRef $ Just (Just curTime, v, p)
        return $ ReactiveInfo v changes mostRecentChange p

  mtv <- liftIO $ readIORef currentRef
  lastChange <- mostRecentChange

  case mtv of
    Just (t, v, p)
      | t > lastChange -> return $ ReactiveInfo v changes mostRecentChange p
    _ -> do
      mtv' <- liftIO $ readIORef futureRef
      case mtv' of
        Just (t', v', p')
          | t' == lastChange -> return $ ReactiveInfo v' changes mostRecentChange p'
        _ -> reRun

instance Functor (EvStream t) where
  fmap f evs = memoEvs (noMemoFmap f evs)

noMemoFmap :: (a -> b) -> EvStream t a -> EvStream t b
noMemoFmap f Never = Never
noMemoFmap f (EvStream sValue) = EvStream $ fmap (fmap f) sValue

instance Functor (Reactive t) where
  fmap f r = memoReactive $ Reactive $ do
    (ReactiveInfo a changes lct se) <- sampleReactive r
    return $ ReactiveInfo (f a) (noMemoFmap f changes) lct se

instance Applicative (Reactive t) where
  pure = return
  (<*>) = ap

instance Monad (Reactive t) where
  return x = Reactive $ return $ ReactiveInfo x never (return Nothing) PNever
  ra >>= f = joinR (fmap f ra)

mostRecent :: Sample t (Maybe Time) -> Sample t (Maybe Time) -> Sample t (Maybe Time)
mostRecent smt1 smt2 = maxOfJust <$> smt1 <*> smt2
  where
    maxOfJust Nothing (Just tl) = Just tl
    maxOfJust (Just tr) Nothing = Just tr
    maxOfJust (Just tl) (Just tr) = Just (max tl tr)
    maxOfJust Nothing Nothing = Nothing

joinR :: forall t a . Reactive t (Reactive t a) -> Reactive t a
joinR rra = Reactive $ do
  (ReactiveInfo ra rraChanges lcrra serra) <- sampleReactive rra
  (ReactiveInfo a raChanges lcra sera) <- sampleReactive ra

  let changesA :: EvStream t a
      changesA = EvStream $ do
        (ReactiveInfo ra rraChanges lcrra serra) <- sampleReactive rra
        rraChangeInfo <- sampleEvStream rraChanges
        case rraChangeInfo of
          NotFired -> do
            (ReactiveInfo a raChanges lcra sera) <- sampleReactive ra
            raChangeInfo <- sampleEvStream raChanges
            case raChangeInfo of
              NotFired -> return NotFired
              FiredNow a' sera' -> return $ FiredNow a' (serra <> sera')

          FiredNow ra' prra' -> do
            (ReactiveInfo a raChanges lcra sera) <- sampleReactive ra'
            raChangeInfo <- sampleEvStream raChanges
            case raChangeInfo of
              NotFired -> return $ FiredNow a (prra' <> sera)
              FiredNow a' sera' -> return $ FiredNow a' (prra' <> sera')

  return $ ReactiveInfo a changesA (mostRecent lcrra lcra) (serra <> sera)

instance MonadFix (Reactive t) where
  mfix f = Reactive $ mfix (\(~(ReactiveInfo a _ _ _)) -> sampleReactive $ f a)

never :: EvStream t a
never = Never

catMaybeEs :: EvStream t (Maybe a) -> EvStream t a
catMaybeEs Never = Never
catMaybeEs (EvStream sampleEvs) = EvStream $ do
  eInfo <- sampleEvs
  case eInfo of
    NotFired -> return NotFired
    FiredNow ma pma -> case ma of
      Nothing -> return NotFired
      Just a -> return $ FiredNow a pma

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
    (FiredNow l pl, FiredNow r pr) -> return $ FiredNow (f l r) (pl <> pr)

switch :: Reactive t (EvStream t a) -> EvStream t a
switch revs = EvStream $ do
  rInfo <- sampleReactive revs
  case currentValue rInfo of
    Never -> return NotFired
    EvStream em' -> em'

runPushes :: BehaviorInternal t a -> Sample t (a, PushStream t)
runPushes = runWriterT

startOnFire :: EvStream t (Behavior t a) -> EvStream t a
startOnFire Never = Never
startOnFire (EvStream sampleEvs) = EvStream $ do
  eInfo <- sampleEvs
  case eInfo of
    NotFired -> return NotFired
    FiredNow (Behavior ba) pba -> do
      (a, pa) <- runPushes ba
      return $ FiredNow a (pba <> pa)

listenPushes :: EvStream t a -> EvStream t (a, PushStream t)
listenPushes Never = Never
listenPushes (EvStream sEInfo) = EvStream $ do
  eInfo <- sEInfo
  case eInfo of
    NotFired -> return NotFired
    FiredNow a p -> return $ FiredNow (a, p) p


holdEs :: a -> EvStream t a -> Behavior t (Reactive t a)
holdEs iv evs = Behavior $ do
  startTime <- lift getCurTime
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

      toPush = catMaybeEs $ startOnFire $ flip fmap (listenPushes evs) $ \(a, p) -> Behavior $ do
                                t <- lift getCurTime
                                if startTime == t
                                then return Nothing
                                else return $ Just (t, a, p)

  tell $ primaryPushes toPush
  tell $ subPushes Nothing PNever

  return $ Reactive $ do
    (v, effects, _) <- liftIO $ readIORef ref
    let lastChange = liftIO $ (\(_,_,sct) -> sct) <$> readIORef ref
    return $ ReactiveInfo v evs lastChange effects

liftReactive :: Reactive t a -> Behavior t a
liftReactive r = Behavior $ do
  (ReactiveInfo v _ _ p) <- lift $ sampleReactive r
  tell p
  return v

liftReactiveAfter :: Reactive t a -> Behavior t a
liftReactiveAfter r = Behavior $ do
  (ReactiveInfo v changes _ p) <- lift $ sampleReactive r
  changeInfo <- lift $ sampleEvStream changes
  case changeInfo of
    NotFired -> tell p >> return v
    FiredNow v' p' -> tell p' >> return v'

changes :: Reactive t a -> EvStream t a
changes r = EvStream $ do
  rInfo <- sampleReactive r
  sampleEvStream $ changesInfo rInfo

newtype Plans t = Plans (EvStream t ())

instance Monoid (Plans t) where
  mempty = Plans never
  mappend (Plans el) (Plans er) = Plans $ mergefEs (<>) el er

data Env = Env { clock :: IO Time
               , scheduleRound :: IO ()
               }
newtype PlanHold t a = PlanHold { unPlanHold :: WriterT (Plans t, PushStream t) (ReaderT Env IO) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

unsafeIOSequence :: EvStream t (IO a) -> IO (EvStream t a)
unsafeIOSequence evs = do
  ref <- newMVar Nothing
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
              FiredNow iov p -> do
                v <- liftIO iov
                t <- getCurTime
                liftIO $ putMVar ref (Just (t, v, p))
                return $ FiredNow v p

  return $ case evs of
            Never -> Never
            EvStream emio -> unsafeEvs emio

{-# NOINLINE unsafeIOMap #-}
unsafeIOMap :: EvStream t (IO a) -> EvStream t a
unsafeIOMap = unsafePerformIO . unsafeIOSequence . fmap liftIO

unsafePlan :: EvStream t (IO a) -> Behavior t (EvStream t a)
unsafePlan evs = Behavior $ do
  plans <- liftIO $ unsafeIOSequence evs
  tell $ evStreamToPushStream $ Just (return ()) <$ plans
  return plans

planEs :: EvStream t (IO a) -> PlanHold t (EvStream t a)
planEs Never = return Never
planEs evs = PlanHold $ do
  plans <- liftIO $ unsafeIOSequence evs
  tell (Plans $ void plans, mempty)
  return plans

liftBehavior :: Behavior t a -> PlanHold t a
liftBehavior b = PlanHold $ do
  iot <- asks clock
  t <- liftIO iot
  (a, p) <- liftIO $ runReaderT (unSample $ runWriterT $ runB b) t
  tell (mempty, p)
  return a

{-

holdEs2 :: forall t a . EvStream t a -> a -> Hold t (Behavior t a)
holdEs2 evs iv = Hold $ do
  startTime <- getCurrentTime

  let toPush = catMaybeEs $ startOnFire $ flip fmap (listenPushes evs) $ \(a, p) -> Hold $ do
                            t <- getCurrentTime
                            if startTime == t
                            then return Nothing
                            else return $ Just (t, a, p)


  toPushRef <- liftIO $ newIORef toPush
  toPushWeakRef <- liftIO $ mkWeakIORef toPushRef (return ())

  let strongPush = EvStream $ do
              evs <- liftIO $ readIORef toPushRef
              case evs of
                Never -> return NotFired
                EvStream es -> es

      weakPush = EvStream $ do
                  evsRef <- liftIO $ deRefWeak toPushWeakRef
                  case evsRef of
                    Just evsR -> do
                      evs <- liftIO $ readIORef evsR
                      case evs of
                        Never -> return NotFired
                        EvStream me -> me
                    Nothing -> return NotFired

  ref <- liftIO $ newIORef (iv, mempty, Nothing)
  weakRef <- liftIO $ mkWeakIORef ref (return ())

  let primaryPushes Never = PNever
      primaryPushes evs@(EvStream sEInfo) = Pls $ do
        mr <- liftIO $ deRefWeak weakRef
        case mr of
          Just r -> do
            eInfo <- sEInfo
            case eInfo of
              NotFired -> return (PNotFired, primaryPushes evs)
              FiredNow (t, newVal, newEffects) _ -> do
                let pushAction = do
                      (_, oldEffects, _) <- readIORef r
                      writeIORef r (newVal, oldEffects <> newEffects, Just t)
                return (PFired pushAction, primaryPushes evs)
          Nothing -> return (PNotFired, PNever)

      subPushes :: PushStream t
      subPushes = Pls $ do
        mr <- liftIO $ deRefWeak weakRef
        case mr of
          Just r -> do
            (_, subEffects, _) <- liftIO $ readIORef r
            case subEffects of
              PNever -> return (PNotFired, PNever)
              Pls mSubEffects -> mSubEffects
          Nothing -> return (PNotFired, PNever)

  addPushStream $ primaryPushes weakPush
  addPushStream subPushes

  return $ Behavior $ do
    (v, effects, _) <- liftIO $ readIORef ref
    addPushStream effects
    let lastChange = liftIO $ (\(_,_,sct) -> sct) <$> readIORef ref
        sFuture = case strongPush of
                    Never -> return NotFired
                    EvStream mToPush -> fmap (fmap (\(_,v,_) -> BehaviorInfo v sFuture lastChange)) mToPush
    return $ BehaviorInfo v sFuture lastChange
-}
