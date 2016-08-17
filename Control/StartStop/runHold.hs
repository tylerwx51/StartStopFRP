{-# LANGUAGE RecursiveDo, BangPatterns #-}
module Control.StartStop.RunHold where

import Control.StartStop.Core
import Control.StartStop.EvPrim
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef
{-
main :: IO ()
main = testHold 100000 $ return (return "")
-}

testPlanHold :: Integer -> (EvStream t Integer -> PlanHold t (Behavior t String)) -> IO ()
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

  let loop !i = do
        trigger <- readIORef clockTriggerRef
        trigger (i + 1)

        actions <- readIORef actionsRef
        writeIORef actionsRef []
        sequence_ actions

        s <- readIORef sampleRef
        --print (i, s)
        unless (i > n) $ loop (i + 1)

  loop 0

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

testPushStream :: (EvStream t Integer -> PushStream t) -> IO ()
testPushStream f = do
  let evs = EvStream $ ask >>= \(T i) -> return (return i)
      pushes = f evs

      loop i PNever = return ()
      loop i (Pls s) = do
        (pInfo, next) <- runReaderT s (T i)
        case pInfo of
          PNotFired -> return ()
          PFired io -> io
        loop (i + 1) next

  loop 0 pushes

testHold :: (Show a) => Integer -> (EvStream t Integer -> Hold t (Behavior t a)) -> IO ()
testHold numRounds hb = do
  (b, pushes) <- runReaderT (runWriterT . unHold $ hb (EvStream $ ask >>= \(T t) -> return (return t))) (T 0)
  loop 0 pushes b
  where
    loop :: (Show a) => Integer -> PushStream t -> Behavior t a -> IO ()
    loop _ PNever _ = return ()
    loop !i (Pls mPush) b = do

      (v, _) <- runReaderT (runWriterT . unHold $ sample b) (T i)
      --liftIO $ print (i, v)

      (didPush, nextPush) <- runReaderT mPush (T i)

      case didPush of
        PNotFired -> return ()
        PFired ioAction -> ioAction

      when (i < numRounds) $ loop (i + 1) nextPush b

holdProgressive :: Hold t (Behavior t a) -> IO (IO a)
holdProgressive hb = do
  clockRef <- newIORef (T 0)

  (b, pushes) <- runReaderT (runWriterT . unHold $ hb) (T 0)
  streamRef <- newIORef pushes

  let next = do
        t <- readIORef clockRef
        pushes <- readIORef streamRef
        case pushes of
          PNever -> return ()
          Pls sEInfo -> do
            (eioaction, nextStream) <- runReaderT sEInfo t
            case eioaction of
              PNotFired -> return ()
              PFired ioaction -> ioaction

            writeIORef streamRef nextStream

        (bInfo, _) <- runReaderT (runWriterT $ unHold $ runB b) t

        modifyIORef clockRef (\(T i) -> T $ i + 1)
        return $ currentVal bInfo

  return next

initPlanHold :: (IO () -> IO ()) -> PlanHold t () -> IO ()
initPlanHold scheduleAction ph = do
  clockRef <- newIORef (T 0)
  streamRef <- newIORef PNever

  rec
    let env = Env (readIORef clockRef) (scheduleAction loop)

        planSample = case planEvs of
                        Never -> return NotFired
                        EvStream me -> me

        loop = do
          t <- readIORef clockRef
          pushes <- readIORef streamRef
          case pushes of
            PNever -> return ()
            (Pls pushesSample) -> do
              runReaderT planSample t
              (eioaction, nextPushes)  <- runReaderT pushesSample t
              case eioaction of
                PNotFired -> return ()
                PFired ioaction -> ioaction

              writeIORef streamRef nextPushes

              modifyIORef clockRef (\(T i) -> T $ i + 1)

    ((), (Plans planEvs, pushes)) <- runReaderT (runWriterT . unPlanHold $ ph) env
    writeIORef streamRef pushes

  return ()
