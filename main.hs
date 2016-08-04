{-# LANGUAGE BangPatterns, RecursiveDo, GeneralizedNewtypeDeriving #-}
import Control.StartStop.Gloss
import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.Chart
import Control.StartStop.RunHold
import Control.StartStop.TestFun
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.IO.Game as Gloss
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable hiding(toList)
import Data.Traversable
import Data.Monoid hiding (Sum(..), Product(..))
import Buttons
import Graphics.Rendering.Chart.Easy as Chart hiding(translate, white, scale)

import Data.IORef
import Data.Maybe
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Product

reallySeqList :: [a] -> b -> b
reallySeqList [] = seq []
reallySeqList (x:xs) = reallySeqList xs

rseq :: [a] -> [a]
rseq xs = reallySeqList xs xs

holdLastNSecs :: Float -> EvStream t Float -> Behavior t a -> Hold t (Behavior t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs' (\vs (t, v) -> rseq $ (t, v) : filter ((> t - holdTime) . fst) vs) (flip (,) <$> b <@> clock) []

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

  return $ fmap decayColorLine bTrail-}

main2 = runGlossHoldIO (InWindow "X-Y Pos" (500, 500) (10, 10)) white 60 $ \tick ev -> do
  let getDelta (EventMotion (dx, dy)) = Just (dx, dy)
      getDelta _ = Nothing

  bTime <- liftHold $ foldEs' (+) tick 0
  let clock = changes bTime
  bMousePos <- liftHold $ holdEs (filterMapEs getDelta $ fmap head ev) (0,0)
  bTrail <- liftHold $ holdLastNSecs 1.2 clock bMousePos

  let mouseClickEvent (EventKey (MouseButton LeftButton) Up _ _) = Just False
      mouseClickEvent (EventKey (MouseButton LeftButton) Down _ _) = Just True
      mouseClickEvent _ = Nothing

  bMouseState <- liftHold $ holdEs (filterMapEs mouseClickEvent $ fmap head ev) False

  let isPEvent (EventKey (Char 'p') Up _ _) = True
      isPEvent _ = False

  bMousePosData <- liftHold $ holdLastNSecs 3 clock (fmap undefined bMousePos)
  --planEs $ fmap (\xs -> mapM_ (\x -> return $! x) xs) $ changes bMousePosData
  bMouseStateData <- liftHold $ holdLastNSecs 3 clock (fmap (\p -> if not p then (0 :: Float) else 200) bMouseState)
  let bPlot1 = fmap (plot . Chart.line "Mouse Held" . return) bMousePosData
      bPlot2 = fmap (plot . Chart.line "Mouse Held" . return) bMouseStateData

  plotAtTime "Chart-Gloss.png" (filterEs isPEvent $ fmap head ev) ((>>) <$> bPlot1 <*> bPlot2)
  planEs $ print "Should plot" <$ filterEs isPEvent (fmap head ev)

  return $ fmap decayColorLine bTrail

main3 :: IO ()
main3 = runGlossHoldIO (InWindow "X-Y Pos" (500, 500) (10, 10)) white 60 $ \tick ev -> liftHold $ do
              bTime <- foldEs' (+) tick 0
              let clock = changes bTime

                  mouseClickEvent (EventKey (MouseButton LeftButton) Up _ pos) = Just pos
                  mouseClickEvent _ = Nothing

                  mouseClickEvs = filterMapEs mouseClickEvent $ fmap head ev

              bLastMouseClick <- holdEs mouseClickEvs (0,0)
              let bMouseClickPic = fmap (translate (negate 240) (negate 240) . scale 0.4 0.4 . text . show) bLastMouseClick

                  getDelta (EventMotion (dx, dy)) = Just (dx, dy)
                  getDelta _ = Nothing
              bMousePos <- holdEs (filterMapEs getDelta $ fmap head ev) (0,0)
              bMousePosData <- holdLastNSecs 5 clock (fmap snd bMousePos)
              let bMousePosPic = fmap (translate 0 0 . scale 50 1 . drawPlot) bMousePosData

              return $ (<>) <$> bMouseClickPic <*> bMousePosPic


data Screen t = Screen { bPic :: Behavior t Picture, bChange :: EvStream t (Screen t) }
main :: IO ()
main = runGlossHoldIO (InWindow "X-Y Pos" (500, 500) (10, 10)) white 60 $ \tick ev -> liftHold $ do
          bTime <- foldEs (+) tick 0
          let clock = changes bTime
          (Screen bm buttonPress) <- mainMenu clock ev
          rec
            let stream = switch active
            active <- holdEs (fmap bChange stream) buttonPress

          switcher bm $ fmap bPic stream

extentClick :: Extent
extentClick = makeExtent 35 5 (-60) (-140)

extentMouseTracker :: Extent
extentMouseTracker = makeExtent 35 5 (200-60) (200-140)

mainMenu :: EvStream t Float -> EvStream t [Event] -> Hold t (Screen t)
mainMenu clock ev = do
  let clickButton = renderButton extentClick "Click Example"
      clickButtonEvent = ffilter (any (isClickedBy extentClick)) ev
      enterClick = startOnFire ((exampleToScreen clock ev =<< (clickExample ev)) <$ clickButtonEvent)

      mouseTrackButton = renderButton extentMouseTracker "Mouse Tracker"
      mouseButtonEvent = ffilter (any (isClickedBy extentMouseTracker)) ev
      enterMouse = startOnFire ((exampleToScreen clock ev =<< (mouseTrackerExample clock ev)) <$ mouseButtonEvent)
  return $ Screen (return $ clickButton <> mouseTrackButton) (leftmost enterClick enterMouse)

clickExample :: EvStream t [Event] -> Hold t (Behavior t Picture)
clickExample ev = do
  let mouseClickEvent (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
      mouseClickEvent _ = Nothing

      mouseClickEvs = filterMap (\xs -> case filterMap mouseClickEvent xs of
                                          [] -> Nothing
                                          (x:_) -> Just x) ev

  bLastMouseClick <- holdEs mouseClickEvs (0,0)
  return $ fmap (translate (negate 240) (negate 240) . scale 0.4 0.4 . text . show) bLastMouseClick

mouseTrackerExample :: EvStream t Float -> EvStream t [Event] -> Hold t (Behavior t Picture)
mouseTrackerExample clock ev = do

  let getDelta (EventMotion (dx, dy)) = Just (dx, dy)
      getDelta _ = Nothing

  bMousePos <- holdEs (filterMap (\xs -> case filterMap getDelta xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev) (0,0)
  bMousePosData <- holdLastNSecs 5 clock (fmap snd bMousePos)
  return $ fmap (translate 0 0 . scale 50 1 . drawPlot) bMousePosData


exampleToScreen :: EvStream t Float -> EvStream t [Event] -> Behavior t Picture -> Hold t (Screen t)
exampleToScreen clock ev bPic = do
  let extentBack = makeExtent (negate 250 + 35) (negate 250 + 5) (250 - 10) (250 - 90)
      backButton = renderButton extentBack "Back"
      backEvent = startOnFire (mainMenu clock ev <$ ffilter (any (isClickedBy extentBack)) ev)

  return $ Screen (fmap (<> backButton) bPic) backEvent

drawPlot :: [(Float, Float)] -> Picture
drawPlot points = color (Gloss.black) . (Gloss.line) $ shiftedPoints
  where
    max_x = maximum $ fmap fst points
    min_x = minimum $ fmap fst points
    shiftedPoints = fmap (\(x,y) -> (x - max_x, y)) points
{-
-}
{-
main = testPlanHold 100000 $ \tick -> liftHold $ do

  bTime <- foldEs' (+) tick 0
  return $ fmap show bTime
-}
