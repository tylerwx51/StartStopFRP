module Control.StartStop.Chart where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.TestFun

import Control.Monad.IO.Class

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

test :: EvStream t x -> Behavior t [(Integer, Integer)] -> PlanHold t ()
test ePlot b = do
  let evs = snapshots b ePlot
  planEs $ flip fmap evs $ \vs -> do
    liftIO $ toFile def "test.png" $ do
      plot $ line "test" [vs]

  return ()

runTest :: IO ()
runTest = testPlanHold 20 $ \evs -> do
  b <- liftHold $ foldEs' (+) evs 0
  time <- liftHold $ holdEs evs 0
  bac <- liftHold $ foldEs' (flip (:)) (startOnFire $ (sample b >>= \v -> sample time >>= \t -> return (t,v)) <$ evs) []
  test (filterEs (==12) evs) bac
  return $ return ""
