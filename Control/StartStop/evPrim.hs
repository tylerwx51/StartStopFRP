module Control.StartStop.EvPrim where

import Control.StartStop.Core
import Control.Monad.Reader

import Data.IORef

callbackStream :: PlanHold t (a -> IO (), EvStream t [a])
callbackStream = do
  cl <- asks clock
  sr <- asks scheduleRound
  firedValsRef <- liftIO $ newIORef []

  let trigger v = do
        (T curTime) <- cl
        let t = T (curTime + 1)
        modifyIORef firedValsRef ((t, v):)
        sr

      evs = EvStream $ do
              t <- ask
              tvs <- liftIO $ readIORef firedValsRef
              let vs = fmap snd . filter (\(t', v) -> t == t') $ tvs
              liftIO $ modifyIORef firedValsRef (filter (\(t', v) -> t <= t'))
              case vs of
                [] -> return NotFired
                _ -> return $ return vs

  _ <- planEs $ return () <$ evs
  return (trigger, evs)
