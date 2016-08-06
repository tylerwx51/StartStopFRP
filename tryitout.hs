module TryItOut (module Control.StartStop.Core, module Control.StartStop.Lib
                , module Graphics.Gloss, module Graphics.Gloss.Interface.IO.Game
                , isKeyboardPressedEvent, isMouseClickEvent, isMouseChange, holdLastNSecs
                , tryItOut
                ) where

import Control.StartStop.Core
import Control.StartStop.Lib
import Control.StartStop.Gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

isKeyboardPressedEvent :: Event -> Maybe Char
isKeyboardPressedEvent (EventKey (Char c) Down _ _) = Just c
isKeyboardPressedEvent _ = Nothing

isMouseClickEvent :: Event -> Maybe (Float, Float)
isMouseClickEvent (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
isMouseClickEvent _ = Nothing

isMouseChange :: Event -> Maybe (Float, Float)
isMouseChange (EventMotion (dx, dy)) = Just (dx, dy)
isMouseChange _ = Nothing

reallySeqList :: [a] -> b -> b
reallySeqList [] = seq []
reallySeqList (x:xs) = reallySeqList xs

rseq :: [a] -> [a]
rseq xs = reallySeqList xs xs

holdLastNSecs :: Float -> EvStream t Float -> Behavior t a -> Hold t (Behavior t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs' (\vs (t, v) -> rseq $ (t, v) : filter ((> t - holdTime) . fst) vs) (flip (,) <$> b <@> clock) []

tryItOut :: (EvStream t Float -> EvStream t Event -> Hold t (Behavior t Picture)) -> IO ()
tryItOut fh = runGlossHoldIO (InWindow "Try It Out" (500, 500) (10, 10)) white 60 $ \tick evs -> liftHold $ do
  bTime <- foldEs (+) tick 0
  fh (changes bTime) (fmap head evs)
