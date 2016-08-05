import Control.Monad
import Control.StartStop.Lib
import Control.StartStop.Core
import Control.StartStop.Gloss

import Data.Monoid

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Gloss

isMouseClickEvent :: Event -> Maybe (Float, Float)
isMouseClickEvent (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
isMouseClickEvent _ = Nothing

main = runGlossHoldIO (InWindow "Tic-Tac-Toe" (500, 500) (10, 10)) white 60 $ \tick ev -> liftHold $ do
          bTime <- foldEs (+) tick 0
          let clock = changes bTime

              mouseClickEvs = filterMap (\xs -> case filterMap isMouseClickEvent xs of
                                                  [] -> Nothing
                                                  (x:_) -> Just x) ev

          bGameState <- foldEs (\gs f -> f gs) ((\(x, y) -> playerPlays (floor (x / 50)) (floor (y / 50))) <$> mouseClickEvs) initGameState
          return $ fmap drawGameState bGameState

data Player = X | O deriving(Eq,Show)
newtype Board = Board [Maybe Player]
data GameState = GameState { turn :: Player, board :: Board, winner :: Maybe Player }

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

initBoard :: Board
initBoard = Board $ replicate 9 Nothing

initGameState :: GameState
initGameState = GameState X initBoard Nothing

lookupBoard :: Int -> Int -> Board -> Maybe Player
lookupBoard x y (Board b) = b !! (x + y * 3)

setBoard :: Int -> Int -> Player -> Board -> Board
setBoard x y p (Board b) = if inBounds then Board $ take ind b ++ [Just p] ++ drop (ind + 1) b else Board b
  where
    ind = x + y * 3
    inBounds = 0 <= x && x <= 2 && 0 <= y && y <= 2

checkForVictor :: Board -> Maybe Player
checkForVictor board = join $ maybeHead $ catMaybes [checkSquares squares | squares <- [row 0, row 1, row 2, col 0, col 1, col 2, diag1, diag2]]
  where
    allEqual [] = Nothing
    allEqual [x] = Just x
    allEqual (x1:x2:xs) =
      if x1 == x2
      then allEqual (x2:xs)
      else Nothing

    checkSquares locs = allEqual $ fmap (\(x,y) -> lookupBoard x y board) locs

    row x = [(x,0), (x,1), (x,2)]
    col y = [(0,y), (1,y), (2,y)]
    diag1 = [(0,0), (1,1), (2,2)]
    diag2 = [(2,0), (1,1), (0,2)]

    maybeHead [] = Nothing
    maybeHead (x:_) = Just x

playerPlays :: Int -> Int -> GameState -> GameState
playerPlays x y gs@(GameState p b w) = case w of
    Nothing -> GameState (otherPlayer p) nextBoard aVictor
    _ -> gs
  where
    nextBoard = setBoard x y p b
    aVictor = checkForVictor nextBoard

drawBoard :: Board -> Picture
drawBoard board = vertLine 50 <> vertLine 100 <> horizLine 50 <> horizLine 100 <> mconcat [translate (fromIntegral x * 50) (fromIntegral y * 50) (drawBox (lookupBoard x y board)) | (x,y) <- (,) <$> [0..2] <*> [0..2]]
  where
    horizLine x = line [(x,0), (x,150)]
    vertLine y = line [(0,y), (150, y)]
    boxSide = 50
    drawBox mPlayer = scale 0.25 0.25 $ case mPlayer of
                        Nothing -> blank
                        Just X -> text "X"
                        Just O -> text "O"

drawGameState :: GameState -> Picture
drawGameState (GameState p board Nothing) = drawBoard board <> translate 0 (negate 50) (scale 0.2 0.2 (text $ "Player's " ++ show p ++ " turn."))
drawGameState (GameState _ board (Just winner)) = drawBoard board <> translate 0 (negate 50) (scale 0.2 0.2 (text $ "Player " ++ show winner ++ " has won!"))
