{-# LANGUAGE Rank2Types #-}

module TryItOut (module Control.StartStop.Class, module Graphics.Gloss
                , module Graphics.Gloss.Interface.IO.Game
                , isKeyboardPressedEvent, isMouseClickEvent, isMouseChange, holdLastNSecs
                , tryItOut, bMousePos, reallySeqList
                ) where

import Control.StartStop.Class
import Control.StartStop.Gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

isKeyboardPressedEvent :: Event -> Maybe Char
isKeyboardPressedEvent (EventKey (Char c) Down _ _) = Just c
isKeyboardPressedEvent _ = Nothing

isMouseClickEvent :: Event -> Maybe Event
isMouseClickEvent e@(EventKey (MouseButton LeftButton) _ _ _) = Just e
isMouseClickEvent _ = Nothing

isMouseChange :: Event -> Maybe (Float, Float)
isMouseChange (EventMotion (dx, dy)) = Just (dx, dy)
isMouseChange _ = Nothing

bMousePos :: (StartStop t) => EvStream t Event -> Behavior t (Reactive t (Float, Float))
bMousePos evs = holdEs (0,0) (filterMap isMouseChange evs)

reallySeqList :: [a] -> b -> b
reallySeqList [] = seq []
reallySeqList (x:xs) = reallySeqList xs

rseq :: [a] -> [a]
rseq xs = reallySeqList xs xs

holdLastNSecs :: (StartStop t) => Float -> EvStream t Float -> Reactive t a -> Behavior t (Reactive t [(Float, a)])
holdLastNSecs holdTime clock b = foldEs (\vs (t, v) -> rseq $ (t, v) : filter ((> t - holdTime) . fst) vs) [] (flip (,) <$> sampleR b <@> clock)

tryItOut :: (forall t . (StartStop t) => EvStream t Float -> EvStream t Event -> Behavior t (Reactive t Picture)) -> IO ()
tryItOut fh = runGlossHoldIO (InWindow "Try It Out" (800, 800) (10, 10)) white 60 $ \tick evs -> sampleB $ do
  bTime <- foldEs (+) 0 tick
  fh (changes bTime) (fmap head evs)
