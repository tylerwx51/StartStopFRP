{-# LANGUAGE RecursiveDo #-}
module Control.StartStop.TestFun where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.EvPrim
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.IORef
import Data.Maybe

main = testHold 100000 test10

testPlanHold n eHold = do
  actionsRef <- newIORef []
  clockTriggerRef <- newIORef undefined
  sampleRef <- newIORef ""

  initPlanHold (\a -> modifyIORef actionsRef (\as -> as ++ [a])) $ do
    (trigger, clock) <- callbackStream
    liftIO $ writeIORef clockTriggerRef trigger
    b <- eHold $ fmap head clock
    let r = startOnFire $ sampleAfter b <$ clock
    planEs $ fmap (writeIORef sampleRef) r
    return ()

  let loop i = do
        trigger <- readIORef clockTriggerRef
        trigger (i + 1)

        actions <- readIORef actionsRef
        writeIORef actionsRef []
        sequence_ actions

        s <- readIORef sampleRef
        print (i, s)
        unless (i > n) $ loop (i + 1)

  loop 0

type TestEvStream a = Time -> Maybe a
type TestBehavior a = Time -> a
type TestHold a = Time -> a

listEvs :: Integer -> TestEvStream a -> [(Time, a)]
listEvs maxTime ev = catMaybes $ do
  t <- fmap T [0..maxTime]
  let mv = ev t
  return $ (\v -> (t, v)) <$> mv

listToEv :: [(Time, a)] -> TestEvStream a
listToEv vs t = lookup t vs

lastFired :: Time -> TestEvStream a -> Maybe (Time, a)
lastFired (T (-1)) _ = Nothing
lastFired t@(T i) ev = case ev t of
                  Nothing -> lastFired (T (i - 1)) ev
                  Just v -> Just (t, v)

neverTest :: TestEvStream a
neverTest _ = Nothing

testSwitch :: TestBehavior (TestEvStream a) -> TestEvStream a
testSwitch b = \t -> b t t

holdTest :: TestEvStream a -> a -> TestHold (TestBehavior a)
holdTest evs iv startTime = \t -> case lastFired t evs of
                                    Just (t',v)
                                      | startTime < t' -> v
                                    _ -> iv

testMergef :: (a -> a -> a) -> TestEvStream a -> TestEvStream a -> TestEvStream a
testMergef f el er t = case (el t, er t) of
  (Just l, Nothing) -> Just l
  (Nothing, Just r) -> Just r
  (Just l, Just r) -> Just $ f l r
  (Nothing, Nothing) -> Nothing

startOnFireTest :: TestEvStream (TestHold a) -> TestEvStream a
startOnFireTest ev t = fmap ($ t) (ev t)

testSample :: TestBehavior a -> TestHold a
testSample = id

testEv :: [(Time, a)] -> Hold t (EvStream t a)
testEv = return . EvStream . impl
  where
    impl :: [(Time, a)] -> Sample t (EvInfo t a)
    impl [] = return NotFired
    impl ((t,v):evs) = do
      curTime <- ask
      if t == curTime
      then return $ FiredNow v mempty
      else impl evs

testFun :: (Time -> Maybe a) -> Hold t (EvStream t a)
testFun = return . EvStream . impl
  where
    impl f = do
      curTime <- ask
      let mv = f curTime
      case mv of
        Nothing -> return NotFired
        Just v -> return $ FiredNow v mempty

testEvUnsafe :: [(Time, a)] -> EvStream t a
testEvUnsafe = EvStream . impl
  where impl :: [(Time, a)] -> Sample t (EvInfo t a)
        impl [] = return NotFired
        impl ((t,v):evs) = do
          curTime <- ask
          if t == curTime
          then return $ FiredNow v mempty
          else impl evs

testFunUnsafe :: (Time -> Maybe a) -> EvStream t a
testFunUnsafe = EvStream . impl
  where
    impl f = do
      curTime <- ask
      let mv = f curTime
      case mv of
        Nothing -> return NotFired
        Just v -> return $ FiredNow v mempty

test1 :: Hold t (Behavior t String)
test1 = do
  evs <- testEv [(T 1,"hello"), (T 5, "world"), (T 9, "lskfj")]
  holdEs evs "Nothing"

test2 :: Hold t (Behavior t String)
test2 = do
  evs <- testEv [(T 3, "foo"), (T 4, "bar"), (T 8, "fun")]
  b <- holdEs (filterEs (/="foo") evs) "___"
  return $ fmap (++"!!") b

test3 :: Hold t (Behavior t String)
test3 = do
  evs <- testEv [(T 4, test1), (T 7, test2)]
  evs2 <- testEv [(T 6, ())]
  t <- test2
  bb <- holdEs (pull $ fmap (fmap Just) evs) t
  let evs3 = pull $ (Just <$> sample bb) <$ evs2
  bb2 <- holdEs evs3 (return "()()")
  return $ join bb2

test4 :: Hold t (Behavior t String)
test4 = do
  ev1 <- testEv [(T 1, "FOO"), (T 4, "BAR"), (T 7, "FUN")]
  ev2 <- testEv [(T 1,"hello"), (T 5, "world"), (T 9, "lskfj")]
  ev3 <- testEv [(T 3, ev1), (T 5, ev2)]

  bes <- holdEs ev3 ev2
  holdEs (switch bes) "_____"

test6 :: Hold t (Behavior t String)
test6 = do
  evs <- testFun (\(T t) -> if odd t then Just (show t) else Nothing)
  holdEs evs "_"

test7 :: Hold t (Behavior t String)
test7 = do
  ev1 <- testEv [(T 1, "FOO"), (T 4, "BAR"), (T 7, "FUN")]
  ev2 <- testEv [(T 4, ())]
  b1 <- holdEs ev1 "_"
  b2 <- holdEs (pull $ (Just <$> sampleAfter b1) <$ ev2) ")))"
  return $ fmap (++"??") b2

test8 :: Hold t (Behavior t [Int])
test8 = do
  ev1 <- testEv [(T 1, 1), (T 6, 9), (T 8, 3)]
  foldEs' (++) (fmap return ev1) []

test9 :: Hold t (Behavior t Integer)
test9 = do
  let update i = testFunUnsafe (\(T t) -> if t == i then Just (*2) else Nothing)
  rec
    let bUps = fmap update b
    b <- foldEs' (\v f -> f v) (switch bUps) 1
  return b

test10 :: Hold t (Behavior t Integer)
test10 = do
  evs <- testFun (\(T i) -> Just i)
  foldEs' (+) (filterEs odd evs) 0

test11 :: Hold t (Behavior t [(Time, Integer)])
test11 = do
  sampler <- testFun (\(T i) -> Just (T i))
  clock <- holdEs sampler (T 0)

  evs <- testEv [(T 1, 1), (T 2, 5), (T 3, 6), (T 10, 100)]
  b <- holdEs evs 0
  foldEs' (\vs v -> vs ++ [v]) ((,) <$> clock <@> changes b) []

testHold :: (Show a) => Integer -> Hold t (Behavior t a) -> IO ()
testHold numRounds hb = do
  (b, Pushes pushes) <- runReaderT (runWriterT . unHold $ hb) (T 0)
  loop 0 pushes b
  where
    loop :: (Show a) => Integer -> EvStream t (IO ()) -> Behavior t a -> IO ()
    loop i (EvStream mPush) b = when (i < numRounds) $ do

      (v, _) <- runReaderT (runWriterT . unHold $ sample b) (T i)
      liftIO $ print (i, v)

      didPush <- runReaderT mPush (T i)

      case didPush of
        NotFired -> loop (i + 1) (EvStream mPush) b
        FiredNow ioAction _ -> do
          ioAction
          loop (i + 1) (EvStream mPush) b

holdProgressive :: Hold t (Behavior t a) -> IO (IO a)
holdProgressive hb = do
  clockRef <- newIORef (T 0)

  rec
    let pushesSample = case pushesEv of
                          Never -> return NotFired
                          EvStream me -> me

        next = do
          t <- readIORef clockRef
          eioaction <- runReaderT pushesSample t
          case eioaction of
            NotFired -> return ()
            FiredNow ioaction _ -> ioaction

          (bInfo, _) <- runReaderT (runWriterT $ unHold $ runB b) t

          modifyIORef clockRef (\(T i) -> T $ i + 1)
          return $ currentVal bInfo

    (b, Pushes pushesEv) <- runReaderT (runWriterT . unHold $ hb) (T 0)

  return next

test5 :: PlanHold t (Behavior t String)
test5 = do
  ev1 <- liftHold $ testEv [(T 1, print "Test1" >> return "FDSL"), (T 6, print "LAKSDJLA" >> return "SLLSL")]
  ev2 <- planEs ev1
  liftHold $ holdEs ev2 "__()"


test12 :: EvStream t Integer -> PlanHold t (Behavior t String)
test12 evs = do
  b <- liftHold $ holdEs evs (negate 1)
  planEs $ print "Cheese" <$ filterEs odd evs
  return $ fmap show b

initPlanHold :: (IO () -> IO ()) -> PlanHold t () -> IO ()
initPlanHold scheduleAction ph = do
  clockRef <- newIORef (T 0)

  rec
    let env = Env (readIORef clockRef) (scheduleAction loop)

        planSample = case planEvs of
                        Never -> return NotFired
                        EvStream me -> me

        pushesSample = case pushesEv of
                          Never -> return NotFired
                          EvStream me -> me

        loop = do
          t <- readIORef clockRef
          runReaderT planSample t
          eioaction <- runReaderT pushesSample t
          case eioaction of
            NotFired -> return ()
            FiredNow ioaction _ -> ioaction

          modifyIORef clockRef (\(T i) -> T $ i + 1)

    ((), (Plans planEvs, Pushes pushesEv)) <- runReaderT (runWriterT . unPlanHold $ ph) env

  return ()
