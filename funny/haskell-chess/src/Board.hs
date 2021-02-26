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
  , KingRookState(..)
  ) where

import Control.Conditional
import Data.Char
import Data.List
import Data.Function
import Data.Maybe
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

data KingRookState
  = Castleable
  | NonCastleable
  deriving (Show)

data Piece
  = Pawn PawnState
  | Rook KingRookState
  | Knight
  | Bishop
  | Queen
  | King KingRookState
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

instance Show Board where
  show (Board b) = show b

aBoard :: [[Square]] -> Board
aBoard squares = (Board (aGenericBoard squares))

figureAt :: Board -> Position -> Square
figureAt (Board board) pos = figureAt' board pos

isKing :: Square -> Bool
isKing (Figure _ (King _)) = True
isKing _                   = False

moveFigure :: Position -> Position -> Board -> Maybe Board
moveFigure fromPosition toPosition b@(Board board) =
  (isKing destination) |>
  (Board
     (board & (putFigure fromPosition Empty) & (putFigure toPosition figure))
     )
  where
    figure = figureAt b fromPosition
    destination = figureAt b toPosition

replaceFigure :: Position -> (Square -> Square) -> Board -> Maybe Board
replaceFigure atPosition updateFigure b@(Board board) =
  isKing figure /=
  isKing figure' |> (Board (board & (putFigure atPosition figure')))
  where
    figure = figureAt b atPosition
    figure' = updateFigure figure

theKing :: Color -> Board -> Position
theKing c b = fromJust (find isTheKing allPositions)
  where
    isTheKing pos = isTheKing' (figureAt b pos)
    isTheKing' (Figure col (King _)) = col == c
    isTheKing' _ = False
