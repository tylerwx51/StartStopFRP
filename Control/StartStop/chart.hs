module Control.StartStop.Chart where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.TestFun

import Control.Monad.IO.Class

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

test :: (PlotValue a, PlotValue b) => EvStream t x -> Behavior t [(b, a)] -> PlanHold t ()
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

test3_ :: (PlotValue a) => EvStream t x -> Behavior t Float -> Behavior t a -> PlanHold t ()
test3_ ePlot time b = do
  let timeChanges = changes time
      hPoints t = do
        v <- sample b
        v' <- sampleAfter b
        return (t,v,v')
  v <- liftHold $ sample b
  t <- liftHold $ sample time
  bPlot <- liftHold $ foldEs' (\vs (t,v,v') -> (t,v'):(t,v):vs) (startOnFire $ hPoints <$> timeChanges) [(t,v)]
  test ePlot bPlot

runTest :: IO ()
runTest = testPlanHold 101 $ \evs -> do
  b <- liftHold $ holdEs (fmap (\t -> sin (fromInteger t/10) :: Float) evs) 0
  time <- liftHold $ holdEs evs 0
  test2_ (filterEs (==100) evs) (fmap fromInteger time) b
  return $ return ""
