module Board
  ( Color(..)
  , opposite
  , Piece(..)
  , Square(..)
  , Board
  , aBoard
  , moveFigure
  , replaceFigure
  , emplaceFigure
  , Position
  , allPositions
  , figureAt
  , theKing
  , Direction(..)
  , moveIn
  , applyDelta
  -- for debug
  , isKing
  , KingRookState(..)
  , replaceAllFigures
  , showPos
  ) where

import Data.Char
import Data.List
import Data.Function
import Data.Maybe
import Direction
import GenericBoard

data Color
  = White
  | Black
  deriving (Eq, Show)

opposite :: Color -> Color
opposite White = Black
opposite Black = White

data KingRookState
  = Castleable
  | NonCastleable
  deriving (Eq, Show, Read)

-- TODO: The concept of King castleability actually doesn't make sense, get rid of it!
data Piece
  = Pawn
  | Rook KingRookState
  | Knight
  | Bishop
  | Queen
  | King KingRookState
  deriving (Eq, Show, Read) -- for debug

data Square
  = Figure Color
           Piece
  | Empty
  | EnPassant

toChar :: Square -> Char
toChar (Figure White Pawn) = 'P'
toChar (Figure White (Rook Castleable)) = 'R'
toChar (Figure White (Rook NonCastleable)) = 'Я'
toChar (Figure White Knight)   = 'N'
toChar (Figure White Bishop)   = 'B'
toChar (Figure White Queen)    = 'Q'
toChar (Figure White (King Castleable)) = 'K'
toChar (Figure White (King NonCastleable)) = 'Y'
toChar (Figure Black piece)    = (toLower . toChar . Figure White) piece
toChar Empty                   = ' '
toChar EnPassant               = '!'

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
  Just (Board
     (board & (putFigure fromPosition Empty) & (putFigure toPosition figure))
     )
  where
    figure = figureAt b fromPosition

replaceFigure :: Position -> (Square -> Square) -> Board -> Board
replaceFigure atPosition updateFigure b@(Board board) =
  Board (board & (putFigure atPosition figure'))
  where
    figure = figureAt b atPosition
    figure' = updateFigure figure

replaceAllFigures :: (Square -> Square) -> Board -> Board
replaceAllFigures upd (Board gb) = Board (mapSquares upd gb)

emplaceFigure :: Position -> Square -> Board -> Maybe Board
emplaceFigure pos sq = Just . replaceFigure pos (\_ -> sq)

theKing :: Color -> Board -> Position
theKing c b = fromJust (find isTheKing allPositions)
  where
    isTheKing pos = isTheKing' (figureAt b pos)
    isTheKing' (Figure col (King _)) = col == c
    isTheKing' _ = False
