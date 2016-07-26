{-# LANGUAGE RecursiveDo #-}
module Control.StartStop.RunHold where

import Control.StartStop.Core
import Control.StartStop.EvPrim
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

main :: IO ()
main = testHold 100000 $ return (return "")

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
