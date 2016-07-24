module Control.StartStop.Chart where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.TestFun

import Control.Monad.IO.Class

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

test :: (PlotValue a) => EvStream t x -> Behavior t [(Integer, a)] -> PlanHold t ()
test ePlot b = do
  let evs = snapshots b ePlot
  planEs $ flip fmap evs $ \vs -> do
    liftIO $ toFile def "test.png" $ do
      plot $ line "test" [vs]

  return ()

test2_ :: (PlotValue a) => EvStream t x -> Behavior t Integer -> Behavior t a -> PlanHold t ()
test2_ ePlot time b = do
  v <- liftHold $ sample b
  t <- liftHold $ sample time
  bac <- liftHold $ foldEs' (flip (:)) (changes $ flip (,) <$> b <*> time) [(t,v)]
  test ePlot bac

runTest :: IO ()
runTest = testPlanHold 20 $ \evs -> do
  b <- liftHold $ foldEs' (+) evs 0
  time <- liftHold $ holdEs evs 0
  test2_ (filterEs (==12) evs) time b
  return $ return ""
