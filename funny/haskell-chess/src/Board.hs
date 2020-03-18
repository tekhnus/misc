module Board
  ( Color(..)
  , opposite
  , PawnState(..)
  , Piece(..)
  , Square(..)
  , Board
  , aBoard
  , moveFigure
  , replaceFigure
  , Position
  , allPositions
  , figureAt
  , theKing
  , Direction(..)
  , moveIn
  -- for debug
  , isKing
  ) where

import Control.Conditional
import Data.Char
import Data.Function
import Direction
import GenericBoard

data Color
  = White
  | Black
  deriving (Eq)

opposite :: Color -> Color
opposite White = Black
opposite Black = White

data PawnState
  = Stable
  | JustMoved
  | Moved
  deriving (Show) -- for debug

data Piece
  = Pawn PawnState
  | Rook Bool
  | Knight
  | Bishop
  | Queen
  | King Bool
  deriving (Show) -- for debug

data Square
  = Figure Color
           Piece
  | Empty

toChar :: Square -> Char
toChar (Figure White (Pawn _)) = 'P'
toChar (Figure White (Rook _)) = 'R'
toChar (Figure White Knight)   = 'N'
toChar (Figure White Bishop)   = 'B'
toChar (Figure White Queen)    = 'Q'
toChar (Figure White (King _)) = 'K'
toChar (Figure Black piece)    = (toLower . toChar . Figure White) piece
toChar Empty                   = ' '

instance Show Square where
  show = (\x -> [x]) . toChar

data Board =
  Board (GenericBoard Square)
        (Color -> Position)

instance Show Board where
  show (Board b _) = show b

aBoard :: [[Square]] -> (Color -> Position) -> Board
aBoard squares k = (Board (aGenericBoard squares) k)

figureAt :: Board -> Position -> Square
figureAt (Board board _) pos = figureAt' board pos

isKing :: Square -> Bool
isKing (Figure _ (King _)) = True
isKing _                   = False

moveFigure :: Position -> Position -> Board -> Maybe Board
moveFigure fromPosition toPosition b@(Board board k) =
  (isKing destination) |>
  (Board
     (board & (putFigure fromPosition Empty) & (putFigure toPosition figure))
     newKing)
  where
    figure = figureAt b fromPosition
    destination = figureAt b toPosition
    newKing = permute . k
    permute pos
      | pos == fromPosition = toPosition
      | otherwise = pos

replaceFigure :: Position -> (Square -> Square) -> Board -> Maybe Board
replaceFigure atPosition updateFigure b@(Board board k) =
  isKing figure /=
  isKing figure' |> (Board (board & (putFigure atPosition figure')) k)
  where
    figure = figureAt b atPosition
    figure' = updateFigure figure

theKing :: Color -> Board -> Position
theKing c (Board _ k) = k c
