{-# LANGUAGE RecursiveDo #-}
module Control.StartStop.Lib where

import Control.StartStop.Core
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

pullAlways :: EvStream t (Hold t a) -> EvStream t a
pullAlways = pull . fmap (fmap Just)

filterMapEs :: (a -> Maybe b) -> EvStream t a -> EvStream t b
filterMapEs f = pull . fmap (return . f)

filterEs :: (a -> Bool) -> EvStream t a -> EvStream t a
filterEs p = filterMapEs boolToJust
  where
    boolToJust v = if p v then Just v else Nothing

data EitherBoth a b = EBLeft a | EBRight b | EBBoth a b
applyEs :: EvStream t (a -> b) -> EvStream t a -> EvStream t b
applyEs ef ea = filterMapEs (\e -> case e of
                                    EBBoth f a -> Just (f a)
                                    _ -> Nothing) $ mergefEs (\(EBLeft f) (EBRight a) -> EBBoth f a) (fmap EBLeft ef) (fmap EBRight ea)

infixl 4 <#>
(<#>) :: EvStream t (a -> b) -> EvStream t a -> EvStream t b
(<#>) = applyEs

gate :: Behavior t Bool -> EvStream t a -> EvStream t a
gate b es = pull $ flip fmap es $ \v -> do
  p <- sample b
  return $ if p then Just v else Nothing

snapshots :: Behavior t a -> EvStream t x -> EvStream t a
snapshots b es = pull $ (Just <$> sample b) <$ es

infixl 4 <@>
(<@>) :: Behavior t (a -> b) -> EvStream t a -> EvStream t b
b <@> es = pull $ flip fmap es $ \a -> do
  f <- sample b
  return $ Just $ f a

forceEval :: EvStream t a -> EvStream t a
forceEval = unsafeIOMap . fmap (\x -> return $! x)

foldEs :: (a -> b -> a) -> EvStream t b -> a -> Hold t (Behavior t a)
foldEs f es iv = do
  rec
    let ups = f <$> b <@> es
    b <- holdEs ups iv
  return b

foldEs' :: (a -> b -> a) -> EvStream t b -> a -> Hold t (Behavior t a)
foldEs' f es iv = do
  rec
    let ups = f <$> b <@> es
    b <- holdEs (forceEval ups) iv
  return b

switchEs :: EvStream t a -> EvStream t (EvStream t a) -> Hold t (EvStream t a)
switchEs iv sw = do
  bes <- holdEs sw iv
  return $ switch bes

switcher :: Behavior t a -> EvStream t (Behavior t a) -> Hold t (Behavior t a)
switcher iv es = do
  bb <- holdEs es iv
  return $ join bb

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
  (b, Pushes pushes) <- runReaderT (runWriterT hb) (T 0)
  loop 0 pushes b
  where
    loop :: (Show a) => Integer -> EvStream t (IO ()) -> Behavior t a -> IO ()
    loop i (EvStream mPush) b = when (i < numRounds) $ do

      (v, _) <- runReaderT (runWriterT $ sample b) (T i)
      liftIO $ print (i, v)

      didPush <- runReaderT mPush (T i)

      case didPush of
        NotFired -> loop (i + 1) (EvStream mPush) b
        FiredNow ioAction _ -> do
          ioAction
          loop (i + 1) (EvStream mPush) b

test5 :: PlanHold t (Behavior t String)
test5 = do
  ev1 <- liftHold $ testEv [(T 1, print "Test1" >> return "FDSL"), (T 6, print "LAKSDJLA" >> return "SLLSL")]
  ev2 <- planEs ev1
  liftHold $ holdEs ev2 "__()"


test12 :: EvStream t Integer -> PlanHold t (Behavior t String)
test12 evs = do
  b <- liftHold $ holdEs evs (negate 1)
  planEs $ print "Cheese" <$ filterEs odd evs
  return $ return ""

initPlanHold :: (IO () -> IO ()) -> PlanHold t () -> IO ()
initPlanHold scheduleAction ph = do
  clockRef <- newIORef (T 0)
  scheduleRef <- newIORef 0

  rec
    let env = Env (readIORef clockRef) (modifyIORef scheduleRef (+1) >> scheduleAction loop)

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
          modifyIORef scheduleRef (subtract 1)

    ((), (Plans planEvs, Pushes pushesEv)) <- runReaderT (runWriterT ph) env

  return ()
