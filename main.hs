{-# LANGUAGE RecursiveDo, BangPatterns #-}
module Main where

import Control.StartStop.Gloss
import Control.StartStop.AnotherCore (runACoreBehavior)
import Control.StartStop.Class
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

holdLastNSecs :: (StartStop t) => Float -> EvStream t Float -> Reactive t a -> Behavior t (Reactive t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs (\vs (t, v) -> rseq $ (t, v) : filter ((> t - holdTime) . fst) vs) [] (flip (,) <$> sampleR b <@> clock)

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

data Screen t = Screen { bPic :: Reactive t Picture, bChange :: EvStream t (Screen t) }

main :: IO ()
main = runGlossHoldIO (InWindow "Examples" (500, 500) (10, 10)) white 60 $ \tick ev -> sampleB $ do
          bTime <- foldEs (+) 0 tick
          let clock = changes bTime
          (Screen bm buttonPress) <- mainMenu clock ev
          rec
            let stream = switch $ sampleR active
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


mainMenu :: (StartStop t) => EvStream t Float -> EvStream t [Event] -> Behavior t (Screen t)
mainMenu clock ev = do
  let clickButton = renderButton extentClick "Click Example"
      clickButtonEvent = void $ ffilter (any (isClickedBy extentClick)) ev
      enterClick = samples ((exampleToScreen clock ev =<< (clickExample ev)) <$ clickButtonEvent)

      mouseTrackButton = renderButton extentMouseTracker "Mouse Tracker"
      mouseButtonEvent = void $ ffilter (any (isClickedBy extentMouseTracker)) ev
      enterMouse = samples ((exampleToScreen clock ev =<< (mouseTrackerExample clock ev)) <$ mouseButtonEvent)

      mouseTrailButton = renderButton extentMouseTrail "Mouse Trail"
      mouseTrailEvent = void $ ffilter (any (isClickedBy extentMouseTrail)) ev
      enterTrail = samples ((exampleToScreen clock ev =<< (mouseTrailExample clock ev)) <$ mouseTrailEvent)

  return $ Screen (return $ clickButton <> mouseTrackButton <> mouseTrailButton) (leftmost [enterClick, enterMouse, enterTrail])

clickExample :: (StartStop t) => EvStream t [Event] -> Behavior t (Reactive t Picture)
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

mouseTrackerExample :: (StartStop t) => EvStream t Float -> EvStream t [Event] -> Behavior t (Reactive t Picture)
mouseTrackerExample clock ev = do
  bMousePos <- holdEs (0, 0) (filterMap (\xs -> case filterMap isMouseChange xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev)
  bMousePosData <- holdLastNSecs 5 clock (fmap snd bMousePos)
  return $ fmap (translate 0 0 . scale 50 1 . drawPlot) bMousePosData

mouseTrailExample :: (StartStop t) => EvStream t Float -> EvStream t [Event] -> Behavior t (Reactive t Picture)
mouseTrailExample clock ev = do
  bMousePos <- holdEs (0,0) (filterMap (\xs -> case filterMap isMouseChange xs of
                                      [] -> Nothing
                                      (x:_) -> Just x) ev)
  bTrail <- holdLastNSecs 1.2 clock bMousePos

  return $ fmap decayColorLine bTrail

exampleToScreen :: (StartStop t) => EvStream t Float -> EvStream t [Event] -> Reactive t Picture -> Behavior t (Screen t)
exampleToScreen clock ev bPic = do
  let extentBack = makeExtent (negate 250 + 35) (negate 250 + 5) (250 - 10) (250 - 90)
      backButton = renderButton extentBack "Back"
      backEvent = samples (mainMenu clock ev <$ ffilter (any (isClickedBy extentBack)) ev)

  return $ Screen (fmap (<> backButton) bPic) backEvent

drawPlot :: [(Float, Float)] -> Picture
drawPlot points = color (Gloss.black) . (Gloss.line) $ shiftedPoints
  where
    max_x = maximum $ fmap fst points
    min_x = minimum $ fmap fst points
    shiftedPoints = fmap (\(x,y) -> (x - max_x, y)) points

constant :: (StartStop t) => a -> Reactive t a
constant = return

main2 = do
  action <- runACoreBehavior $ \tick -> do
    rTime <- foldEs (\(!a) (!b) -> a + b) 0 tick
    return $ fmap show rTime

  mapM_ (\_ -> action) [0..1000000]
