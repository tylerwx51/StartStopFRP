import Control.StartStop.Gloss
import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.Chart
import Control.StartStop.RunHold
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid

holdLastNSecs :: Float -> EvStream t Float -> Behavior t a -> Hold t (Behavior t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs' (\vs (t, v) -> (t, v) : filter ((> t - holdTime) . fst) vs) (flip (,) <$> b <@> clock) []

decayColorLine :: [(Float, (Float, Float))] -> Picture
decayColorLine vs = foldl' (flip (<>)) mempty $ fmap (\(t, (x, y)) -> color (timeColor t) $ translate x y $ circleSolid 10) $ vs
  where
    minTime = minimum $ fmap fst vs
    maxTime = maximum $ fmap fst vs

    timeDif = maxTime - minTime
    timeRed t = (t - minTime - 0.1) / (timeDif + 0.1)
    timeBlue t = (maxTime - t) / (timeDif + 0.05)
    timeColor t = makeColor (timeRed t) (timeBlue t) 0 1

    pair = zipWith (\(t, pos1) (_, pos2) -> (t, [pos1, pos2])) vs (drop 1 vs)

{-main = runGlossHoldIO (InWindow "FDSLK" (500,500) (10, 10)) white 60 $ \tick ev -> liftHold $ do
  let getDelta (EventMotion (dx, dy)) = Just (dx, dy)
      getDelta _ = Nothing

  bTime <- foldEs' (+) tick 0
  let clock = changes bTime
  bMousePos <- holdEs (filterMapEs getDelta ev) (0,0)
  bTrail <- holdLastNSecs 1.2 clock bMousePos

  return $ fmap decayColorLine bTrail
  -}

main = runGlossHoldIO (InWindow "X-Y Pos" (500, 500) (10, 10)) white 60 $ \tick ev -> do
  let getDelta (EventMotion (dx, dy)) = Just (dx, dy)
      getDelta _ = Nothing

  bTime <- liftHold $ foldEs' (+) tick 0
  let clock = changes bTime
  bMousePos <- liftHold $ holdEs (filterMapEs getDelta ev) (0,0)
  bTrail <- liftHold $ holdLastNSecs 1.2 clock bMousePos

  let mouseClickEvent (EventKey (MouseButton LeftButton) Up _ _) = Just True
      mouseClickEvent (EventKey (MouseButton LeftButton) Down _ _) = Just False
      mouseClickEvent _ = Nothing

  bMouseState <- liftHold $ holdEs (filterMapEs mouseClickEvent ev) False

  let isPEvent (EventKey (Char 'p') Up _ _) = True
      isPEvent _ = False

  bPlot1 <- liftHold $ smoothPlot bTime (fmap fst bMousePos)
  bPlot2 <- liftHold $ stepPlot bTime (fmap (\p -> if p then (0 :: Float) else 200) bMouseState)
  plotAtTime "Chart-Gloss.png" (filterEs isPEvent ev) ((>>) <$> bPlot1 <*> bPlot2)
  planEs $ print "Should plot" <$ filterEs isPEvent ev
  return $ fmap decayColorLine bTrail

{-
main = testPlanHold 100000 $ \tick -> do

  bTime <- liftHold $ foldEs' (+) tick 0
  let clock = changes bTime
  --bMousePos <- holdEs ev ""

  return $ fmap show bTime
-}
