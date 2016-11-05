{-# LANGUAGE TypeFamilies
            ,GeneralizedNewtypeDeriving
            , ScopedTypeVariables
            , DeriveFunctor
            , GADTs
            , Rank2Types
            , TypeOperators
            , RecursiveDo #-}

module Control.StartStop.AnotherCore where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Arrow
import Control.StartStop.Class (FilterFunctor, filterMap, catMaybes, ffilter, (.:), FireTime(..))
import qualified Control.StartStop.Class as Class

import Data.IORef
import Data.Functor.Compose
import Data.Function
import Data.Unique
import Data.Map.Strict hiding(filter)
import qualified Data.Map.Strict as Map

import System.Mem.Weak
import System.IO.Unsafe

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

type MemoEvRef t a = IORef (Maybe (Time, FireInfo t a))
data EvStream t a where
  Never :: EvStream t a
  EvMap :: (a -> b) -> EvStream t a -> EvStream t b
  ECatMaybes :: EvStream t (Maybe a) -> EvStream t a
  Samples :: EvStream t (Behavior t a) -> EvStream t a
  Switch :: Behavior t (EvStream t a) -> EvStream t a
  Merge :: EvStream t a -> EvStream t b -> EvStream t (FireTime a b)
  ActionEv :: ReadPhase t (FireInfo t a) -> EvStream t a
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

noMemoBMap :: (a -> b) -> Behavior t a -> Behavior t b
noMemoBMap f (BConstant v) = BConstant (f v)
noMemoBMap f (BMap g b) = BMap (f . g) b
noMemoBMap f b = BMap f b

noMemoRMap :: (a -> b) -> Reactive t a -> Reactive t b
noMemoRMap f (RConstant v) = RConstant (f v)
noMemoRMap f (RMap g b) = RMap (f . g) b
noMemoRMap f b = RMap f b

noMemoEMap :: (a -> b) -> EvStream t a -> EvStream t b
noMemoEMap f Never = Never
noMemoEMap f (EvMap g evs) = EvMap (f . g) evs
noMemoEMap f evs = EvMap f evs

never :: EvStream t a
never = Never

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



listenCensor :: (MonadWriter w m) => m a -> m (a, w)
listenCensor = censor (const mempty) . listen

newtype ReadPhase t a = ReadPhase { runReadPhase :: ReaderT Time IO a } deriving (Functor,Applicative,Monad,MonadFix)

readIORefRead :: IORef a -> ReadPhase t a
readIORefRead = ReadPhase . liftIO . readIORef

newIORefRead :: a -> ReadPhase t (IORef a)
newIORefRead = ReadPhase . liftIO . newIORef

getCurTime :: ReadPhase t Time
getCurTime = ReadPhase ask

newUniqueRead :: ReadPhase t Unique
newUniqueRead = ReadPhase $ liftIO newUnique

newtype WritePhase t a = WritePhase { runWritePhase :: IO a } deriving (Functor,Applicative,Monad)

writeIORefWrite :: IORef a -> a -> Phase t ()
writeIORefWrite ref v = Compose $ return $ Compose $ (WritePhase $ writeIORef ref v) >> (return $ pure ())

newtype CleanUpPhase t a = CleanUpPhase { runCleanUpPhase :: IO a } deriving (Functor,Applicative,Monad)

writeIORefClean :: IORef a -> a -> CleanUpPhase t ()
writeIORefClean ref v = CleanUpPhase $ writeIORef ref v

runAndCleanUp :: (RoundSequence t -> CleanUpPhase t ()) -> RoundSequence t -> Phase t ()
runAndCleanUp f rs = Compose $ do
  writePhase <- getCompose $ runRoundSequence rs
  return $ Compose $ do
    cleanUpPhase <- getCompose writePhase
    return $ do
      rsNew <- cleanUpPhase
      f rsNew

type Phase t a = (ReadPhase t `Compose` WritePhase t `Compose` CleanUpPhase t) a
data RoundAction t = NoAction | RoundAction (Phase t (RoundAction t))
newtype RoundSequence t = RoundSequence (Map Unique (RoundAction t))

isNoAction :: RoundAction t -> Bool
isNoAction NoAction = True
isNoAction _ = False

runRoundAction :: RoundAction t -> Phase t (RoundAction t)
runRoundAction NoAction = pure NoAction
runRoundAction (RoundAction p) = p

runRoundSequence :: RoundSequence t -> Phase t (RoundSequence t)
runRoundSequence (RoundSequence phaseMap) = RoundSequence . Map.filter (not . isNoAction) <$> traverse runRoundAction phaseMap

newRoundSequence :: RoundAction t -> ReadPhase t (RoundSequence t)
newRoundSequence action = do
  identifier <- newUniqueRead
  return $ RoundSequence $ singleton identifier action

runPhase :: Phase t a -> ReaderT Time IO a
runPhase phase = do
  writePhase <- runReadPhase $ getCompose phase
  cleanUpPhase <- lift $ runWritePhase $ getCompose writePhase
  lift $ runCleanUpPhase cleanUpPhase

instance Monoid (RoundAction t) where
  mempty = NoAction
  mappend NoAction r = r
  mappend r NoAction = r
  mappend (RoundAction lphase) (RoundAction rphase) = RoundAction $ liftA2 (<>) lphase rphase

instance Monoid (RoundSequence t) where
  mempty = RoundSequence mempty
  mappend (RoundSequence lphaseMap) (RoundSequence rphaseMap) = RoundSequence $ lphaseMap <> rphaseMap

runBehavior :: forall a . (forall t . EvStream t Integer -> Behavior t (Reactive t a)) -> IO (IO a)
runBehavior f = do
  let tick = ActionEv $ do
              (Time i) <- getCurTime
              return $ FiredNow i mempty
      breactive = f tick
  clock <- newIORef (Time 0)
  (reactive, rs) <- runReaderT (runReadPhase (runWriterT (runB breactive))) (Time 0)
  currentRSRef <- newIORef rs

  return $ do
    time <- readIORef clock
    (v,_) <- runReaderT (runReadPhase (runWriterT (runR reactive))) time
    rs <- readIORef currentRSRef

    newrs <- runReaderT (runPhase $ runRoundSequence rs) time
    writeIORef currentRSRef rs

    modifyIORef clock succ
    return v

createWeakStrongPair :: a -> IO (IORef a, Weak (IORef a))
createWeakStrongPair iv = do
  ref <- newIORef iv
  weakRef <- mkWeakIORef ref (return ())
  return (ref, weakRef)

createWeakStrongEvPair :: EvStream t a -> IO (EvStream t a, EvStream t a)
createWeakStrongEvPair evs = do
  (ref, weakRef) <- createWeakStrongPair evs
  let strongEvs = ActionEv $ do
                    es <- readIORefRead ref
                    evHoldInfo es

      weakEvs = ActionEv $ do
                  mres <- ReadPhase $ liftIO $ deRefWeak weakRef
                  case mres of
                    Nothing -> return NotFired
                    Just res -> do
                      es <- readIORefRead res
                      evHoldInfo es

  return (strongEvs, weakEvs)

runB :: Behavior t a -> WriterT (RoundSequence t) (ReadPhase t) a
runB (BConstant v) = return v
runB (BMap f b) = fmap f $ runB b
runB (BJoin bba) = runB bba >>= runB
runB (BFix f) = mfix (runB . f)
runB (SampleR r) = runR r
runB (SampleRAfter r) = runRAfter r
runB (HoldEs iv evs) = do
  startTime <- lift getCurTime
  (activeValueRef, activeValueWeakRef) <- lift $ ReadPhase $ liftIO $ createWeakStrongPair iv
  activeRSRef <- lift $ newIORefRead mempty
  lastChangeRef <- lift $ newIORefRead Nothing

  (strongEvs, weakEvs) <- lift $ ReadPhase $ liftIO $ createWeakStrongEvPair evs

  let readFires = evHoldInfo weakEvs
      changeAction = RoundAction $ Compose $ do
        mAValueref <- ReadPhase $ liftIO $ deRefWeak activeValueWeakRef

        case mAValueref of
          Nothing -> return $ pure NoAction
          Just ref -> do
            t <- getCurTime
            fireInfo <- readFires
            case fireInfo of
              FiredNow v rs
                | startTime /= t -> getCompose $  writeIORefWrite ref v
                                               *> writeIORefWrite lastChangeRef (Just t)
                                               *> runAndCleanUp (writeIORefClean activeRSRef) rs
                                               *> pure changeAction

              _ -> do
                activeRS <- readIORefRead activeRSRef
                getCompose $  runAndCleanUp (writeIORefClean activeRSRef) activeRS
                           *> pure changeAction

  thisRS <- lift $ newRoundSequence changeAction
  tell thisRS

  let sampleAction = do
        v <- readIORefRead activeValueRef
        rs <- readIORefRead activeRSRef
        return (v, rs)

      lastChange = readIORefRead lastChangeRef

  return $ HeldEs strongEvs lastChange sampleAction

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
                  | FiredNow !a (RoundSequence t)
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
evHoldInfo (ActionEv action) = action
evHoldInfo (Samples ev) = do
  fireInfo <- evHoldInfo ev
  case fireInfo of
    NotFired -> return NotFired
    FiredNow b rs -> do
      (v, brs) <- runWriterT $ runB b
      return $ FiredNow v (rs <> brs)
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
    cOuter = samples $ sampleRAfter <$> changes rra
    cInner = switch $ sampleR $ fmap changes rra
changesHoldInfo (HeldEs changes _ _) = evHoldInfo changes
changesHoldInfo (MemoR _ _ r) = changesHoldInfo r

type PlanIO t a = IO a

data PlanHold t a where
  CallbackStream :: PlanHold t (a -> IO (), EvStream t [a])
  PlanEs :: EvStream t (PlanIO t a) -> PlanHold t (EvStream t a)
  PlanPure :: a -> PlanHold t a
  PlanMap :: (a -> b) -> PlanHold t a -> PlanHold t b
  PlanJoin :: PlanHold t (PlanHold t a) -> PlanHold t a
  PSampleB :: Behavior t a -> PlanHold t a
  PFix :: (a -> PlanHold t a) -> PlanHold t a
  PLiftIO :: IO a -> PlanHold t a

instance Functor (PlanHold t) where
  fmap f (PlanPure x) = PlanPure (f x)
  fmap f (PlanJoin ppa) = PlanJoin $ fmap (fmap f) ppa
  fmap f (PlanMap g pa) = PlanMap (f . g) pa
  fmap f pa = PlanMap f pa

instance Applicative (PlanHold t) where
  pure = return
  (<*>) = ap

instance Monad (PlanHold t) where
  return = PlanPure
  (PlanPure a) >>= f = f a
  pa >>= f = PlanJoin (fmap f pa)

instance MonadFix (PlanHold t) where
  mfix = PFix

instance MonadIO (PlanHold t) where
  liftIO = PLiftIO

noMemoPMap :: (a -> b) -> PlanHold t a -> PlanHold t b
noMemoPMap f (PlanPure v) = PlanPure (f v)
noMemoPMap f (PlanMap g p) = PlanMap (f . g) p
noMemoPMap f p = PlanMap f p

callbackStream :: PlanHold t (a -> IO (), EvStream t [a])
callbackStream = CallbackStream

sampleB :: Behavior t a -> PlanHold t a
sampleB = PSampleB

planEs :: EvStream t (PlanIO t a) -> PlanHold t (EvStream t a)
planEs = PlanEs

data Env = Env { clock :: IO Time
               , scheduleRound :: IO ()
               }
newtype Plans t = Plans (EvStream t ())

instance Monoid (Plans t) where
  mempty = Plans never
  mappend (Plans e1) (Plans e2) = Plans $ mergeWith (\_ _ -> ()) e1 e2

reallySeqList :: [a] -> b -> b
reallySeqList [] = seq []
reallySeqList (x:xs) = reallySeqList xs

runPlanHoldImpl :: PlanHold t a -> WriterT (Plans t, RoundSequence t) (ReaderT Env IO) a
runPlanHoldImpl (PlanPure v) = return v
runPlanHoldImpl (PlanMap f pa) = f <$> runPlanHoldImpl pa
runPlanHoldImpl (PlanJoin ppa) = runPlanHoldImpl ppa >>= runPlanHoldImpl
runPlanHoldImpl (PlanEs evsIO) = do
  ref <- liftIO $ newIORef Nothing
  let evs = ActionEv $ do
        mtv <- readIORefRead ref
        curTime <- getCurTime
        case mtv of
          Just (t, fireInfo)
            | t == curTime -> return fireInfo
          _ -> do
            fireInfo <- evHoldInfo evsIO
            case fireInfo of
              NotFired -> do
                ReadPhase $ liftIO $ writeIORef ref (Just (curTime, NotFired))
                return NotFired
              FiredNow ioa rs -> do
                a <- ReadPhase $ liftIO ioa
                ReadPhase $ liftIO $ writeIORef ref (Just (curTime, FiredNow a rs))
                return $ FiredNow a rs

  tell (Plans $ void evs, mempty)
  return evs
runPlanHoldImpl CallbackStream = do
  cl <- asks clock
  sr <- asks scheduleRound
  currentValRef <- liftIO $ newIORef Nothing
  futureValRef <- liftIO $ newIORef []

  let trigger v = do
        (Time curTime) <- cl
        let t = Time (curTime + 1)
        modifyIORef' futureValRef ((t, v):)
        sr

      evs = ActionEv $ do
              t <- getCurTime
              mtvs <- readIORefRead currentValRef
              vs <- case mtvs of
                Just (t', vs)
                  | t' == t -> return vs
                _ -> do
                  tvs <- readIORefRead futureValRef
                  let vs' = fmap snd . filter (\(t', v) -> t == t') $ tvs
                  ReadPhase $ liftIO $ writeIORef currentValRef (Just (t, vs'))
                  ReadPhase $ liftIO $ modifyIORef' futureValRef (\vs -> let vs' = filter (\(t', v) -> t < t') vs in reallySeqList vs' vs')
                  return vs'

              --vs <- mapM return vs' -- fixes some leak that is caused by laziness with vs'

              case vs of
                [] -> return NotFired
                _ -> return $ FiredNow vs mempty

  _ <- runPlanHoldImpl $ PlanEs $ return () <$ evs
  return (trigger, evs)

runPlanHoldImpl (PSampleB b) = do
  clockRef <- asks clock
  t <- liftIO clockRef
  (v, rs) <- liftIO $ runReaderT (runReadPhase $ runWriterT (runB b)) t
  tell (mempty, rs)
  return v

runPlanHoldImpl (PFix f) = mfix (runPlanHoldImpl . f)
runPlanHoldImpl (PLiftIO ioa) = liftIO ioa

initPlanHold :: (IO () -> IO ()) -> (forall t . PlanHold t ()) -> IO ()
initPlanHold scheduleAction ph = do
  clockRef <- newIORef (Time 0)
  streamRef <- newIORef mempty

  rec
    let env = Env (readIORef clockRef) (scheduleAction loop)

        loop = do
          t <- readIORef clockRef
          rs <- readIORef streamRef
          nextRS <- runReaderT (runPhase $ runRoundSequence rs) t
          writeIORef streamRef nextRS
          _ <- runReaderT (runReadPhase $ evHoldInfo planEvs) t
          modifyIORef clockRef (\(Time i) -> Time $ i + 1)

    (_, (Plans planEvs, rs)) <- runReaderT (runWriterT . runPlanHoldImpl $ ph) env
    writeIORef streamRef rs

  return ()

data ACore t
instance Class.StartStop (ACore t) where
  newtype Behavior (ACore t) a = B { unB :: Behavior t a } deriving (Functor, Applicative, Monad, MonadFix)
  newtype Reactive (ACore t) a = R { unR :: Reactive t a } deriving (Functor, Applicative, Monad)
  newtype EvStream (ACore t) a = E { unE :: EvStream t a } deriving (Functor, FilterFunctor)

  never = E never
  merge e1 e2 = E $ merge (unE e1) (unE e2)
  samples = E . samples . noMemoEMap unB . unE
  switch = E . switch . noMemoBMap unE . unB
  coincidence = E . coincidence . noMemoEMap unE . unE
  holdEs iv evs = B $ noMemoBMap R $ holdEs iv (unE evs)
  sampleR = B . sampleR . unR
  sampleRAfter = B . sampleRAfter . unR
  changes = E . changes . unR


instance Class.StartStopIO (ACore t) where
  newtype PlanIO (ACore t) a = PIO { unPIO :: IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)
  newtype PlanHold (ACore t) a = P { unP :: PlanHold t a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

  callbackStream = P $ noMemoPMap (second E) callbackStream
  planEs = P . noMemoPMap E . planEs . noMemoEMap unPIO . unE
  sampleB = P . sampleB . unB


runACoreBehavior :: (forall t . (Class.StartStop t) => Class.EvStream t Integer -> Class.Behavior t (Class.Reactive t a)) -> IO (IO a)
runACoreBehavior f = runBehavior (noMemoBMap unR . unB . f . E)

testACoreBehavior :: Integer -> (forall t . (Class.StartStop t) => Class.EvStream t Integer -> Class.Behavior t (Class.Reactive t a)) -> IO [a]
testACoreBehavior n f = do
  nextAction <- runACoreBehavior f
  mapM (const nextAction) [0..n]

initACorePlanHold :: (IO () -> IO ()) -> (forall t . (Class.StartStopIO t) => Class.PlanHold t ()) -> IO ()
initACorePlanHold schedule p = initPlanHold schedule (unP p)
