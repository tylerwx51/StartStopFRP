{-# LANGUAGE RecursiveDo #-}

module Control.StartStop.Run where

import Control.StartStop.Core
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

runBehavior :: (Show a) => Integer -> (EvStream t Integer -> Behavior t (Reactive t a)) -> IO ()
runBehavior numRounds br = do
  (r, pushes) <- runReaderT (unSample $ runWriterT . runB $ br (EvStream $ getCurTime >>= \(T t) -> return (FiredNow t PNever))) (T 0)
  loop 0 pushes r
  where
    loop :: (Show a) => Integer -> PushStream t -> Reactive t a -> IO ()
    loop _ PNever _ = return ()
    loop i (Pls mPush) r = do

      (v, _) <- runReaderT (unSample $ runWriterT $ runB $ liftReactive r) (T i)
      --liftIO $ print (i, v)

      (didPush, nextPush) <- runReaderT (unSample mPush) (T i)

      case didPush of
        PNotFired -> return ()
        PFired ioAction -> ioAction

      when (i < numRounds) $ loop (i + 1) nextPush r

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
              runReaderT (unSample planSample) t
              (eioaction, nextPushes)  <- runReaderT (unSample pushesSample) t
              case eioaction of
                PNotFired -> return ()
                PFired ioaction -> ioaction

              writeIORef streamRef nextPushes

              modifyIORef clockRef (\(T i) -> T $ i + 1)

    ((), (Plans planEvs, pushes)) <- runReaderT (runWriterT . unPlanHold $ ph) env
    writeIORef streamRef pushes

  return ()
