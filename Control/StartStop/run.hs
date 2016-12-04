{-# LANGUAGE RecursiveDo, Rank2Types #-}

module Control.StartStop.Run where

import qualified Control.StartStop.Class as Class
import Control.StartStop.Core
import Control.StartStop.Prim
import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Data.IORef

runBehavior :: forall a . (forall t . EvStream t Integer -> Behavior t (Reactive t a)) -> IO (IO a)
runBehavior f = do
  let tick = EvStream $ do
              (T i) <- getCurTime
              return $ FiredNow i mempty
      breactive = f tick
  clock <- newIORef (T 0)
  (TimeValue reactive _ rs) <- runReaderT (unSample $ runB breactive) (T 0)
  currentRSRef <- newIORef rs

  return $ do
    time <- readIORef clock
    (TimeValue v _ _) <- runReaderT (unSample $ sampleCurrent reactive) time
    rs <- readIORef currentRSRef

    newrs <- runReaderT (runPhase $ runRoundSequence rs) time
    writeIORef currentRSRef rs

    modifyIORef clock (\(T i) -> T (i + 1))
    return v

runACoreBehavior :: (forall t . (Class.StartStop t) => Class.EvStream t Integer -> Class.Behavior t (Class.Reactive t a)) -> IO (IO a)
runACoreBehavior f = runBehavior (noMemoBMap unR . unB . f . E)

initPlanHold :: (IO () -> IO ()) -> (forall t . PlanHold t ()) -> IO ()
initPlanHold scheduleAction ph = do
  clockRef <- newIORef (T 0)
  streamRef <- newIORef mempty

  rec
    let env = Env (readIORef clockRef) (scheduleAction loop)

        planSample = case planEvs of
                        Never -> return NotFired
                        EvStream me -> me

        loop = do
          t <- readIORef clockRef
          rs <- readIORef streamRef
          next <- runReaderT (runPhase (runRoundSequence rs)) t
          writeIORef streamRef next

          runReaderT (unSample planSample) t
          modifyIORef clockRef (\(T i) -> T $ i + 1)

    ((), (Plans planEvs, rs)) <- runReaderT (runWriterT . unPlanHold $ ph) env
    writeIORef streamRef rs

  return ()

initCorePlanHold :: (IO () -> IO ()) -> (forall t . (Class.StartStopIO t) => Class.PlanHold t ()) -> IO ()
initCorePlanHold sr ph = initPlanHold sr (unP ph)
