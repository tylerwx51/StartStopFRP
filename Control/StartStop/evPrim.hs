module Control.StartStop.EvPrim where

import Control.StartStop.Core
import Control.Monad.Reader

import Data.IORef

reallySeqList :: [a] -> b -> b
reallySeqList [] = seq []
reallySeqList (x:xs) = reallySeqList xs

callbackStream :: (Show a) => PlanHold t (a -> IO (), EvStream t [a])
callbackStream = do
  cl <- PlanHold $ asks clock
  sr <- PlanHold $ asks scheduleRound
  currentValRef <- liftIO $ newIORef Nothing
  futureValRef <- liftIO $ newIORef []

  let trigger v = do
        (T curTime) <- cl
        let t = T (curTime + 1)
        modifyIORef' futureValRef ((t, v):)
        sr

      evs = EvStream $ do
              t <- getCurTime
              mtvs <- liftIO $ readIORef currentValRef
              vs <- case mtvs of
                Just (t', vs)
                  | t' == t -> return vs
                _ -> do
                  tvs <- liftIO $ readIORef futureValRef
                  let vs' = fmap snd . filter (\(t', v) -> t == t') $ tvs
                  liftIO $ writeIORef currentValRef (Just (t, vs'))
                  liftIO $ modifyIORef' futureValRef (\vs -> let vs' = filter (\(t', v) -> t < t') vs in reallySeqList vs' vs')
                  return vs'

              --vs <- mapM return vs' -- fixes some leak that is caused by laziness with vs'

              case vs of
                [] -> return NotFired
                _ -> return $ return vs

  _ <- planEs $ return () <$ evs
  return (trigger, evs)
