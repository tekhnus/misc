{-# LANGUAGE LambdaCase #-}

module Rules
  ( State(..)
  , makeMove
  , newGame
  -- for debug
  , basicMoves, basicMovesFromPosition, positionsAtDirection, makeBasicMove, moveFigure'
  ) where

import Board
import Control.Conditional
import Data.Function
import Data.Maybe
import NewBoard

positionsAtDirection :: Position -> Int -> Direction -> [Position]
positionsAtDirection _ 0 _ = []
positionsAtDirection position dist direction =
  case (moveIn direction position) of
    Nothing -> []
    Just position' ->
      position' : (positionsAtDirection position' (dist - 1) direction)

update :: Square -> Square
update (Figure color (Pawn Stable))    = Figure color (Pawn JustMoved)
update (Figure color (Pawn JustMoved)) = Figure color (Pawn Moved)
update (Figure color (Rook Castleable))      = Figure color (Rook NonCastleable)
update (Figure color (King Castleable))      = Figure color (King NonCastleable)
update figure                          = figure

moveFigure' :: Position -> Position -> Board -> Maybe Board
moveFigure' fromPosition toPosition board = do
  board' <- board & (moveFigure fromPosition toPosition)
  replaceFigure toPosition update board'

data State =
  State Color
        Board

type BasicMove = (Position, Position)

data Castling =
  KingsideCastling | QueensideCastling
  deriving (Eq, Read)

type Move = Either BasicMove Castling

instance Show State where
  show (State _ board) = show board

makeBasicMove :: BasicMove -> State -> Maybe State
makeBasicMove (fromPosition, toPosition) (State color board) = do
  board' <- (board & (moveFigure' fromPosition toPosition))
  return (State (opposite color) board')

availablePositionsAtDirection :: State -> Position -> Int -> Direction -> [Position]
availablePositionsAtDirection _ _ 0 _ = []
availablePositionsAtDirection (State color board) pos dist dir =
  let
    posList = positionsAtDirection pos dist dir
    pairs = zip posList (tail posList)
    nicePair (p, prevP) =
      let fig = figureAt board p
          prevFig = figureAt board prevP
       in case (fig, prevFig) of
            (_, Figure _ _) -> False
            (Figure fcolor _, _) | fcolor == color -> False
            _ -> True
   in map fst (takeWhile nicePair pairs)

basicMovesFromPosition :: State -> Position -> [(BasicMove, State)]
basicMovesFromPosition state@(State color board) position =
  case (figureAt board position) of
    Figure fcolor piece | fcolor == color ->
      let moveStatePair pos' = do
            let m = (position, pos')
            s' <- makeBasicMove m state 
            return (m, s')
          simpleMoves dist directions st po = (concatMap (availablePositionsAtDirection st po dist) directions)
          positionFunction = case piece of
            Rook _ -> simpleMoves 8 [B, W, K, Q]
            Bishop -> simpleMoves 8 [BK, BQ, WK, WQ]
            Queen -> simpleMoves 8 [B, W, K, Q, BK, BQ, WK, WQ]
            King _ -> simpleMoves 1 [B, W, K, Q, BK, BQ, WK, WQ]
            Knight -> (\_ _ -> [])
            Pawn _ -> (\_ _ -> [])
          positions = positionFunction state position
       in catMaybes (map moveStatePair positions)
    _ -> []

basicMoves :: State -> [(BasicMove, State)]
basicMoves state = concatMap (basicMovesFromPosition state) allPositions

castlingKingPath :: Castling -> [Int]
castlingKingPath KingsideCastling = [4, 5, 6]
castlingKingPath QueensideCastling = [4, 3, 2]

castlingRookPath :: Castling -> [Int]
castlingRookPath KingsideCastling = [7, 6, 5]
castlingRookPath QueensideCastling = [0, 1, 2, 3]

localToGlobal :: Color -> Int -> Position
localToGlobal White n = (0, n)
localToGlobal Black n = (7, 7 - n)

attacked :: Color -> Position -> Board -> Bool
attacked color position board = any attack mvs
  where
    state = State (opposite color) board
    mvs = basicMoves state
    attack ((_, x'), _) = x' == position

kingAttackedOn :: Color -> Position -> Board -> Bool
kingAttackedOn color position board =
  case mboard' of
    Just board' -> attacked color position board'
    Nothing     -> True
  where
    mboard' = moveFigure' king position board
    king = theKing color board

kingAttacked :: Color -> Board -> Bool
kingAttacked color board = kingAttackedOn color (theKing color board) board

makeCastling :: Castling -> State -> Maybe State
makeCastling castling (State color board) = do
  let localToGlobal' = localToGlobal color
      attacked' ps = kingAttackedOn color (localToGlobal' ps) board
      empty' = (\case Empty -> True; _ -> False) . (figureAt board) . localToGlobal'
      kingPath = castlingKingPath castling
      rookPath = castlingRookPath castling
      king = head kingPath
      rook = head rookPath
      king' = last kingPath
      rook' = last rookPath
  guard (case figureAt board (localToGlobal' king) of
    Figure col (King Castleable) | col == color -> True
    _ -> False)
  guard (case figureAt board (localToGlobal' rook) of
    Figure col (Rook Castleable) | col == color -> True
    _ -> False)
  guard (all (not . attacked') (kingPath))
  guard (all (empty' <||> (== rook)) (tail kingPath))
  guard (all (empty' <||> (== king)) (tail rookPath))
  board'' <- moveFigure' (localToGlobal' king) (localToGlobal' king') board
  board' <- moveFigure' (localToGlobal' rook) (localToGlobal' rook') board''
  return (State (opposite color) board')

castlings :: State -> [(Castling, State)]
castlings state = do
  castling <- [KingsideCastling]
  state' <- maybeToList (makeCastling castling state)
  return (castling, state')


makeMove :: Move -> State -> Maybe State
makeMove turn state@(State color _) = do
  state'@(State _ board') <-
    case turn of
      Left move       -> lookup move (basicMoves state)
      Right castling -> lookup castling (castlings state)
  (kingAttacked color board') |> state'

newGame :: State
newGame = State White newBoard
