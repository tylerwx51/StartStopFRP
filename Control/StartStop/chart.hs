module Control.StartStop.Chart where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.RunHold

import Control.Monad.IO.Class

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

plotAtTime :: (ToRenderable a, Default a) => FilePath -> EvStream t x -> Behavior t (EC a ()) -> PlanHold t ()
plotAtTime path ePlot b = do
  let evs = snapshots b ePlot
  planEs $ flip fmap evs $ \vs -> liftIO $ toFile def path vs

  return ()

smoothPlot :: Behavior t Integer -> Behavior t a -> Hold t (Behavior t (EC (Layout Integer a) ()))
smoothPlot time b = do
  v <- sample b
  t <- sample time
  bac <- foldEs' (flip (:)) (changes $ flip (,) <$> b <*> time) [(t,v)]
  return $ fmap (\vs -> plot $ line "Smooth Plot" [vs]) bac

stepPlot :: Behavior t Float -> Behavior t a -> Hold t (Behavior t (EC (Layout Float a) ()))
stepPlot time b = do
  let timeChanges = changes time
      hPoints t = do
        v <- sample b
        v' <- sampleAfter b
        return (t,v,v')
  v <- sample b
  t <- sample time
  bPoints <- foldEs' (\vs (t,v,v') -> (t,v'):(t,v):vs) (startOnFire $ hPoints <$> timeChanges) [(t,v)]
  return $ fmap (\vs -> plot $ line "Step Plot" [vs]) bPoints


plotEvStream :: Behavior t Float -> EvStream t a -> Hold t (Behavior t (EC (Layout Float a) ()))
plotEvStream time e = do
  bPoints <- foldEs (\vs (t,v) -> (t,v):vs) ((,) <$> time <@> e) []
  return $ fmap (plot . points "test") bPoints

runTest :: IO ()
runTest = testPlanHold 101 $ \evs -> do
  b <- liftHold $ holdEs (fmap (\t -> sin (fromInteger t/10) :: Float) evs) 0
  time <- liftHold $ holdEs evs 0
  bPlot <- liftHold $ plotEvStream (fmap fromInteger time) (changes b)
  plotAtTime "test.png" (filterEs (==100) evs) bPlot
  return $ return ""

readMeExample1 :: IO ()
readMeExample1 = testPlanHold 101 $ \evs -> do
  b <- liftHold $ holdEs (fmap (\x -> x * x) evs) 0
  time <- liftHold $ holdEs evs 0
  bPlot <- liftHold $ smoothPlot (fmap fromInteger time) b
  plotAtTime "Ex1.png" (filterEs (==100) evs) bPlot
  return $ return ""
