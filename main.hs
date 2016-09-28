{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.StartStop.Gloss
import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.Run
import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.IO.Game as Gloss

import Data.Foldable hiding(toList)
import Data.Monoid hiding (Sum(..), Product(..))

import Buttons

reallySeqList :: [a] -> b -> b
reallySeqList [] = seq []
reallySeqList (x:xs) = reallySeqList xs

rseq :: [a] -> [a]
rseq xs = reallySeqList xs xs

holdLastNSecs :: Float -> EvStream t Float -> Reactive t a -> Behavior t (Reactive t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs (\vs (t, v) -> rseq $ (t, v) : filter ((> t - holdTime) . fst) vs) [] (flip (,) <$> b <@> clock)

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

{-
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
-}

data Screen t = Screen { bPic :: Reactive t Picture, bChange :: EvStream t (Screen t) }
main2 :: IO ()
main2 = runGlossHoldIO (InWindow "Examples" (500, 500) (10, 10)) white 60 $ \tick ev -> liftBehavior $ do
          bTime <- foldEs (+) 0 tick
          let clock = changes bTime
          (Screen bm buttonPress) <- mainMenu clock ev
          rec
            let stream = switch active
            active <- holdEs buttonPress (fmap bChange stream)

          switcher bm $ fmap bPic stream

extentClick :: Extent
extentClick = makeExtent 35 5 (-60) (-200)

extentMouseTracker :: Extent
extentMouseTracker = makeExtent 35 5 (200-60) (200-200)

extentMouseTrail :: Extent
extentMouseTrail = makeExtent (negate 10) (negate 40) (negate 60) (negate 200)

isMouseClickEvent :: Event -> Maybe (Float, Float)
isMouseClickEvent (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
isMouseClickEvent _ = Nothing

isMouseChange :: Event -> Maybe (Float, Float)
isMouseChange (EventMotion (dx, dy)) = Just (dx, dy)
isMouseChange _ = Nothing

mainMenu :: EvStream t Float -> EvStream t [Event] -> Behavior t (Screen t)
mainMenu clock ev = do
  let clickButton = renderButton extentClick "Click Example"
      clickButtonEvent = void $ ffilter (any (isClickedBy extentClick)) ev
      enterClick = startOnFire ((exampleToScreen clock ev =<< (clickExample ev)) <$ clickButtonEvent)

      mouseTrackButton = renderButton extentMouseTracker "Mouse Tracker"
      mouseButtonEvent = void $ ffilter (any (isClickedBy extentMouseTracker)) ev
      enterMouse = startOnFire ((exampleToScreen clock ev =<< (mouseTrackerExample clock ev)) <$ mouseButtonEvent)

      mouseTrailButton = renderButton extentMouseTrail "Mouse Trail"
      mouseTrailEvent = void $ ffilter (any (isClickedBy extentMouseTrail)) ev
      enterTrail = startOnFire ((exampleToScreen clock ev =<< (mouseTrailExample clock ev)) <$ mouseTrailEvent)

  return $ Screen (return $ clickButton <> mouseTrackButton <> mouseTrailButton) (foldr1 leftmost [enterClick, enterMouse, enterTrail])

clickExample :: EvStream t [Event] -> Behavior t (Reactive t Picture)
clickExample ev = do
  -- filter ev down to only the first mouse click event.
  let mouseClickEvs = filterMap (\xs -> case filterMap isMouseClickEvent xs of
                                          [] -> Nothing
                                          (x:_) -> Just x) ev

  -- creates a behavior with a value of the last clicks position
  bLastMouseClick <- holdEs (0,0) mouseClickEvs

  -- Takes a position and makes a Picture of text with the value of the position
  let positionToText = translate (negate 240) (negate 240) . scale 0.4 0.4 . text . show

  -- return a behavior whos current value is the current screen to draw.
  return $ fmap positionToText bLastMouseClick

mouseTrackerExample :: EvStream t Float -> EvStream t [Event] -> Behavior t (Reactive t Picture)
mouseTrackerExample clock ev = do
  bMousePos <- holdEs (0, 0) (filterMap (\xs -> case filterMap isMouseChange xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev)
  bMousePosData <- holdLastNSecs 5 clock (fmap snd bMousePos)
  return $ fmap (translate 0 0 . scale 50 1 . drawPlot) bMousePosData

mouseTrailExample :: EvStream t Float -> EvStream t [Event] -> Behavior t (Reactive t Picture)
mouseTrailExample clock ev = do
  bMousePos <- holdEs (0,0) (filterMap (\xs -> case filterMap isMouseChange xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev)
  bTrail <- holdLastNSecs 1.2 clock bMousePos

  return $ fmap decayColorLine bTrail

exampleToScreen :: EvStream t Float -> EvStream t [Event] -> Reactive t Picture -> Behavior t (Screen t)
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

main = runBehavior 1000000 $ \tick -> do
  bTime <- switcher (return 0) $ startOnFire $ fmap (\_ -> holdEs 0 tick) $ ffilter (\x -> x `rem` 3 == 0) tick
  return $ fmap show bTime
