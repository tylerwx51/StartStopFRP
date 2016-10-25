{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, ScopedTypeVariables, DeriveFunctor, GADTs, Rank2Types, DeriveFunctor, RecursiveDo #-}
module Control.StartStop.AnotherCore where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Arrow
import Data.IORef
import Data.Functor.Compose

import System.IO.Unsafe

class FilterFunctor f where
  filterMap :: (a -> Maybe b) -> f a -> f b
  {--
  - laws :
  - filterMap Just = id
  - filterMap f . filterMap g = filterMap (f <=< g)
  --}

newtype Time = Time Integer deriving (Eq,Ord,Enum,Show)

type MemoBRef t a = IORef (Maybe (Time, (a, RoundSequence t)))
data Behavior t a where
  BConstant :: a -> Behavior t a
  HoldEs :: a -> EvStream t a -> Behavior t (Reactive t a)
  SampleR :: Reactive t a -> Behavior t a
  SampleRAfter :: Reactive t a -> Behavior t a
  BMap :: (a -> b) -> Behavior t a -> Behavior t b
  BJoin :: Behavior t (Behavior t a) -> Behavior t a
  BFix :: (a -> Behavior t a) -> Behavior t a
  MemoB :: MemoBRef t a -> Behavior t a -> Behavior t a

-- bfix must be isomorphic to
-- bfix f = \t -> fix (\v -> f v t)

data FireTime a b = LeftFired a | RightFired b | Simul a b
type MemoEvRef t a = IORef (Maybe (Time, FireInfo t a))
data EvStream t a where
  Never :: EvStream t a
  EvMap :: (a -> b) -> EvStream t a -> EvStream t b
  ECatMaybes :: EvStream t (Maybe a) -> EvStream t a
  Samples :: EvStream t (Behavior t a) -> EvStream t a
  Switch :: Behavior t (EvStream t a) -> EvStream t a
  Merge :: EvStream t a -> EvStream t b -> EvStream t (FireTime a b)
  ActionEv :: (Time -> IO a) -> EvStream t a
  Changes :: Reactive t a -> EvStream t a
  Coincidence :: EvStream t (EvStream t a) -> EvStream t a
  MemoEv :: MemoEvRef t a -> EvStream t a -> EvStream t a

type MemoRRef t a = IORef (Maybe (Time, a, RoundSequence t, ReadPhase t (Maybe Time)))
data Reactive t a where
  RMap :: (a -> b) -> Reactive t a -> Reactive t b
  RConstant :: a -> Reactive t a
  RJoin :: Reactive t (Reactive t a) -> Reactive t a
  HeldEs :: EvStream t a -> ReadPhase t (Maybe Time) -> ReadPhase t (a, RoundSequence t) -> Reactive t a
  MemoR :: MemoRRef t a -> MemoRRef t a -> Reactive t a -> Reactive t a

{-# NOINLINE memoB #-}
memoB :: Behavior t a -> Behavior t a
memoB (BConstant v) = BConstant v
memoB b@(MemoB _ _) = b
memoB b = unsafePerformIO $ do
  ref <- newIORef Nothing
  return $ MemoB ref b

instance Functor (Behavior t) where
  fmap f (BConstant v) = BConstant (f v)
  fmap f (BMap g b) = BMap (f . g) b
  fmap f (BJoin bba) = BJoin $ fmap (fmap f) bba
  fmap f b = memoB $ BMap f b

instance Applicative (Behavior t) where
  pure = return
  (<*>) = ap

instance Monad (Behavior t) where
  return = BConstant
  b >>= k = bJoin $ fmap k b

bJoin :: Behavior t (Behavior t a) -> Behavior t a
bJoin (BConstant ba) = ba
bJoin (BJoin bba) = BJoin (BMap BJoin bba)
bJoin bba = BJoin bba

instance MonadFix (Behavior t) where
  mfix = BFix

{-# NOINLINE memoEv #-}
memoEv :: EvStream t a -> EvStream t a
memoEv b = unsafePerformIO $ do
  ref <- newIORef Nothing
  return $ MemoEv ref b

instance Functor (EvStream t) where
  fmap f Never = Never
  fmap f evs = memoEv $ EvMap f evs

instance FilterFunctor (EvStream t) where
  filterMap f = ECatMaybes . fmap f

{-# NOINLINE memoR #-}
memoR :: Reactive t a -> Reactive t a
memoR b = unsafePerformIO $ do
  ref <- newIORef Nothing
  futureRef <- newIORef Nothing
  return $ MemoR ref futureRef b

instance Functor (Reactive t) where
  fmap f (RConstant v) = RConstant (f v)
  fmap f r = memoR $ RMap f r

instance Applicative (Reactive t) where
  pure = return
  (<*>) = ap

instance Monad (Reactive t) where
  return = RConstant
  b >>= k = RJoin $ fmap k b


merge :: EvStream t a -> EvStream t b -> EvStream t (FireTime a b)
merge Never ev = EvMap RightFired ev
merge ev Never = EvMap LeftFired ev
merge lev rev = Merge lev rev

mergeWith :: (a -> a -> a) -> EvStream t a -> EvStream t a -> EvStream t a
mergeWith f lev rev = fmap cmp $ merge lev rev
  where
    cmp (LeftFired l) = l
    cmp (RightFired r) = r
    cmp (Simul l r) = f l r

switch :: Behavior t (EvStream t a) -> EvStream t a
switch (BConstant ev) = ev
switch bev = Switch bev

holdEs :: a -> EvStream t a -> Behavior t (Reactive t a)
holdEs iv Never = return (return iv)
holdEs iv evs = HoldEs iv evs

sampleR :: Reactive t a -> Behavior t a
sampleR (RConstant v) = return v
sampleR r = SampleR r

sampleRAfter :: Reactive t a -> Behavior t a
sampleRAfter (RConstant v) = return v
sampleRAfter r = SampleR r

samples :: EvStream t (Behavior t a) -> EvStream t a
samples Never = Never
samples evs = Samples evs

changes :: Reactive t a -> EvStream t a
changes (RConstant _) = Never
changes r = Changes r

coincidence :: EvStream t (EvStream t a) -> EvStream t a
coincidence Never = Never
coincidence eevs = Coincidence eevs



tick :: EvStream t Integer
tick = ActionEv $ \(Time i) -> return i

listenCensor :: (MonadWriter w m) => m a -> m (a, w)
listenCensor = censor (const mempty) . listen

newtype ReadPhase t a = ReadPhase { runReadPhase :: ReaderT Time IO a } deriving (Functor,Applicative,Monad,MonadFix)

readIORefRead :: IORef a -> ReadPhase t a
readIORefRead = ReadPhase . liftIO . readIORef

newIORefRead :: a -> ReadPhase t (IORef a)
newIORefRead = ReadPhase . liftIO . newIORef

getCurTime :: ReadPhase t Time
getCurTime = ReadPhase ask

newtype WritePhase t a = WritePhase { runWritePhase :: IO a } deriving (Functor,Applicative,Monad)

writeIORefWrite :: IORef a -> a -> Phase t ()
writeIORefWrite ref v = Compose $ return $ Compose $ (WritePhase $ writeIORef ref v) >> (return $ pure ())

newtype CleanUpPhase t a = CleanUpPhase { runCleanUpPhase :: IO a } deriving (Functor,Applicative,Monad)

writeIORefClean :: IORef a -> a -> CleanUpPhase t ()
writeIORefClean ref v = CleanUpPhase $ writeIORef ref v

runAndCleanUp :: (RoundSequence t -> CleanUpPhase t ()) -> RoundSequence t -> Phase t ()
runAndCleanUp f NoChange = Compose $ return $ Compose $ return $ f NoChange
runAndCleanUp f rs@(RoundSequence phase) = Compose $ do
  writePhase <- getCompose phase
  return $ Compose $ do
    cleanUpPhase <- getCompose writePhase
    return $ do
      rsNew <- cleanUpPhase
      f rsNew

type Phase t a = (ReadPhase t `Compose` WritePhase t `Compose` CleanUpPhase t) a
data RoundSequence t = NoChange | RoundSequence (Phase t (RoundSequence t))

runPhase :: Phase t a -> ReaderT Time IO a
runPhase phase = do
  writePhase <- runReadPhase $ getCompose phase
  cleanUpPhase <- lift $ runWritePhase $ getCompose writePhase
  lift $ runCleanUpPhase cleanUpPhase

instance Monoid (RoundSequence t) where
  mempty = NoChange
  mappend NoChange rs = rs
  mappend rs NoChange = rs
  mappend (RoundSequence lphase) (RoundSequence rphase) = RoundSequence $ liftA2 (<>) lphase rphase

runBehavior :: forall a . (forall t . EvStream t Integer -> Behavior t (Reactive t a)) -> IO (IO a)
runBehavior f = do
  let breactive = f tick
  clock <- newIORef (Time 0)
  (reactive, rs) <- runReaderT (runReadPhase (runWriterT (runB breactive))) (Time 0)
  currentRSRef <- newIORef rs

  return $ do
    time <- readIORef clock
    (v,_) <- runReaderT (runReadPhase (runWriterT (runR reactive))) time
    rs <- readIORef currentRSRef

    case rs of
      NoChange -> return ()
      RoundSequence phase -> do
        newrs <- runReaderT (runPhase phase) time
        writeIORef currentRSRef rs

    modifyIORef clock succ
    return v

foldEs :: (a -> b -> a) -> a -> EvStream t b -> Behavior t (Reactive t a)
foldEs f iv evs = do
  rec
    let ups = samples $ fmap (\b -> sampleR rAns >>= \a -> return (f a b)) evs
    rAns <- holdEs iv ups
  return rAns

testBehavior :: Integer -> forall a . (forall t . EvStream t Integer -> Behavior t (Reactive t a)) -> IO [a]
testBehavior n f = do
  nextAction <- runBehavior f
  mapM (const nextAction) [0..n]

runB :: Behavior t a -> WriterT (RoundSequence t) (ReadPhase t) a
runB (BConstant v) = return v
runB (BMap f b) = fmap f $ runB b
runB (BJoin bba) = runB bba >>= runB
runB (BFix f) = mfix (runB . f)
runB (SampleR r) = runR r
runB (SampleRAfter r) = runRAfter r
runB (HoldEs iv evs) = do
  startTime <- lift getCurTime
  activeValueRef <- lift $ newIORefRead iv
  activeRSRef <- lift $ newIORefRead mempty
  lastChangeRef <- lift $ newIORefRead Nothing

  let readFires = evHoldInfo evs
      changeSequence = RoundSequence $ Compose $ do
        t <- getCurTime
        fireInfo <- readFires
        case fireInfo of
          FiredNow v rs
            | startTime /= t -> getCompose $  writeIORefWrite activeValueRef v
                                           *> writeIORefWrite activeRSRef rs
                                           *> writeIORefWrite lastChangeRef (Just t)
                                           *> pure changeSequence
          _ -> return $ pure changeSequence

      activeSequence = RoundSequence $ Compose $ do
        activeRS <- readIORefRead activeRSRef
        getCompose $  runAndCleanUp (writeIORefClean activeRSRef) activeRS
                   *> pure activeSequence

  tell changeSequence
  tell activeSequence

  let sampleAction = do
        v <- readIORefRead activeValueRef
        rs <- readIORefRead activeRSRef
        return (v, rs)

      lastChange = readIORefRead lastChangeRef

  return $ HeldEs evs lastChange sampleAction

runB (MemoB bref b) = lift (memoReadPhase bref (runWriterT (runB b))) >>= writer

memoReadPhase :: IORef (Maybe (Time, a)) -> ReadPhase t a -> ReadPhase t a
memoReadPhase ref reRun = do
  mtv <- readIORefRead ref
  curTime <- getCurTime

  case mtv of
    Just (t, v)
      | t == curTime -> return v
    _ -> do
      v <- reRun
      ReadPhase $ lift $ writeIORef ref (Just (curTime, v))
      return v

{-
MemoR :: MemoRRef t a -> Reactive t a -> Reactive t a
-}
runR :: Reactive t a -> WriterT (RoundSequence t) (ReadPhase t) a
runR r = do
  (v, rs, _) <- lift $ runRT r
  tell rs
  return v

mostRecent :: ReadPhase t (Maybe Time) -> ReadPhase t (Maybe Time) -> ReadPhase t (Maybe Time)
mostRecent smt1 smt2 = maxOfJust <$> smt1 <*> smt2
  where
    maxOfJust Nothing (Just tl) = Just tl
    maxOfJust (Just tr) Nothing = Just tr
    maxOfJust (Just tl) (Just tr) = Just (max tl tr)
    maxOfJust Nothing Nothing = Nothing

runRT :: Reactive t a -> ReadPhase t (a, RoundSequence t, ReadPhase t (Maybe Time))
runRT (RConstant v) = return (v, mempty, return Nothing)
runRT (RMap f r) = fmap (\(v, rs, lc) -> (f v, rs, lc)) $ runRT r
runRT (RJoin rra) = do
  (ra, rs1, lc1) <- runRT rra
  (a, rs2, lc2) <- runRT ra
  return (a, rs1 <> rs2, mostRecent lc1 lc2)
runRT (HeldEs _ lc sample) = do
  (v, rs) <- sample
  return (v, rs, lc)
runRT (MemoR _ _ r) = runRT r

runRAfter :: Reactive t a -> WriterT (RoundSequence t) (ReadPhase t) a
runRAfter (RConstant v) = return v
runRAfter (RMap f r) = fmap f $ runRAfter r
runRAfter (RJoin rra) = runRAfter rra >>= runRAfter
runRAfter (HeldEs changes _ sample) = do
  changeInfo <- lift $ evHoldInfo changes
  case changeInfo of
    NotFired -> lift sample >>= \(v, rs) -> writer (v, rs)
    FiredNow v rs -> tell rs >> return v
runRAfter (MemoR _ _ r) = runRAfter r

data FireInfo t a = NotFired
                  | FiredNow a (RoundSequence t)
                  deriving (Functor)

mergeFireInfo :: FireInfo t a -> FireInfo t b -> FireInfo t (FireTime a b)
mergeFireInfo NotFired NotFired = NotFired
mergeFireInfo fl NotFired = fmap LeftFired fl
mergeFireInfo NotFired fr = fmap RightFired fr
mergeFireInfo (FiredNow lv lrs) (FiredNow rv rrs) = FiredNow (Simul lv rv) (lrs <> rrs)

flatten :: FireInfo t (Maybe a) -> FireInfo t a
flatten (FiredNow Nothing _) = NotFired
flatten (FiredNow (Just v) rs) = FiredNow v rs

evHoldInfo :: EvStream t a -> ReadPhase t (FireInfo t a)
evHoldInfo Never = return NotFired
evHoldInfo (EvMap f ev) = fmap f <$> evHoldInfo ev
evHoldInfo (Merge lev rev) = mergeFireInfo <$> evHoldInfo lev <*> evHoldInfo rev
evHoldInfo (ECatMaybes ev) = flatten <$> evHoldInfo ev
evHoldInfo (ActionEv action) = fmap (\v -> FiredNow v mempty) $ ReadPhase $ ReaderT action
evHoldInfo (Samples ev) = do
  fireInfo <- evHoldInfo ev
  case fireInfo of
    NotFired -> return NotFired
    FiredNow b rs -> do
      (v, brs) <- runWriterT $ runB b
      return $ FiredNow v brs
evHoldInfo (Switch bev) = do
  (ev, brs) <- runWriterT $ runB bev
  fireInfo <- evHoldInfo ev
  case fireInfo of
    NotFired -> return NotFired
    FiredNow v rs -> return $ FiredNow v (brs <> rs)
evHoldInfo (Changes r) = changesHoldInfo r
evHoldInfo (Coincidence eevs) = do
  outerFire <- evHoldInfo eevs
  case outerFire of
    NotFired -> return NotFired
    FiredNow evs outerRS -> do
      innerFire <- evHoldInfo evs
      case innerFire of
        NotFired -> return NotFired
        FiredNow v innerRS -> return $ FiredNow v (outerRS <> innerRS)

evHoldInfo (MemoEv evRef evs) = memoReadPhase evRef (evHoldInfo evs)


changesHoldInfo :: Reactive t a -> ReadPhase t (FireInfo t a)
changesHoldInfo (RConstant _) = return NotFired
changesHoldInfo (RMap f r) = fmap f <$> changesHoldInfo r
changesHoldInfo (RJoin rra) = evHoldInfo $ mergeWith const cOuter cInner
  where
    cOuter = samples $ fmap sampleRAfter $ changes rra
    cInner = switch $ sampleR $ fmap changes rra
changesHoldInfo (HeldEs changes _ _) = evHoldInfo changes
changesHoldInfo (MemoR _ _ r) = changesHoldInfo r
