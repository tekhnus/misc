module Rules
  ( State(..)
  , makeMove
  , newGame
  -- for debug
  , basicMoves, basicMovesFromPosition, touch, moveDirections, movesAtDirection, positionsAtDirection, distance, makeBasicMove, moveFigure'
  ) where

import Board
import Control.Conditional
import Data.Function
import Data.Maybe
import NewBoard

moveDirections :: Color -> Piece -> [Direction]
moveDirections White (Pawn _) = [B]
moveDirections Black (Pawn _) = [W]
moveDirections _ (Rook _)     = [B, W, K, Q]
moveDirections _ Knight       = []
moveDirections _ Bishop       = [BK, BQ, WK, WQ]
moveDirections _ Queen        = [B, W, K, Q, BK, BQ, WK, WQ]
moveDirections _ (King _)     = [B, W, K, Q, BK, BQ, WK, WQ]

takeDirections :: Color -> Piece -> [Direction]
takeDirections White (Pawn _) = [BK, BQ]
takeDirections Black (Pawn _) = [WK, WQ]
takeDirections color piece    = moveDirections color piece

distance :: Piece -> Int
distance (Pawn _) = 1
distance (King _) = 1
distance _        = 8

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
  KingsideCastling
  deriving (Eq, Read)

type Move = Either BasicMove Castling

instance Show State where
  show (State _ board) = show board

makeBasicMove :: BasicMove -> State -> Maybe State
makeBasicMove (fromPosition, toPosition) (State color board) = do
  board' <- (board & (moveFigure' fromPosition toPosition))
  return (State (opposite color) board')

movesAtDirection :: State -> Position -> Int -> Direction -> [(BasicMove, State)]
movesAtDirection state@(State _ board) position dist direction =
  untilStop positions
  where
    positions = positionsAtDirection position dist direction
    untilStop [] = []
    untilStop (p:ps) =
      case (figureAt board p) of
        Empty ->
          case makeBasicMove (position, p) state of
            Just state' -> ((position, p), state') : (untilStop ps)
            Nothing     -> untilStop ps
        _ -> []

takesAtDirection :: State -> Position -> Int -> Direction -> [(BasicMove, State)]
takesAtDirection state@(State color board) position dist direction =
  untilStop positions
  where
    positions = positionsAtDirection position dist direction
    untilStop [] = []
    untilStop (p:ps) =
      case (figureAt board p) of
        Empty -> untilStop ps
        (Figure fcolor _) ->
          if fcolor /= color
            then case makeBasicMove (position, p) state of
                   Just state' -> [((position, p), state')]
                   Nothing     -> []
            else []

touch :: State -> Position -> Maybe Piece
touch (State color board) position =
  case (figureAt board position) of
    Figure color' piece
      | color' == color -> Just piece
    _ -> Nothing

basicMovesFromPosition :: State -> Position -> [(BasicMove, State)]
basicMovesFromPosition state@(State color _) position =
  case (touch state position) of
    Just piece ->
      let moveDirections' = moveDirections color piece
          takeDirections' = takeDirections color piece
          distance' = distance piece
          movesAtDirection' = movesAtDirection state position distance'
          takesAtDirection' = takesAtDirection state position distance'
       in (concatMap movesAtDirection' moveDirections') ++
          (concatMap takesAtDirection' takeDirections')
    Nothing -> []

basicMoves :: State -> [(BasicMove, State)]
basicMoves state = concatMap (basicMovesFromPosition state) allPositions

castlingKingPath :: Castling -> [Int]
castlingKingPath KingsideCastling = [4, 5, 6]

castlingRookPath :: Castling -> [Int]
castlingRookPath KingsideCastling = [7, 5]

atHomeRow :: Color -> Int -> Position
atHomeRow _ _ = (0, 0)

empty :: Position -> Board -> Bool
empty position board =
  case figureAt board position of
    Empty -> True
    _     -> False

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
  let localToGlobal = atHomeRow color
      attacked' ps = kingAttackedOn color (localToGlobal ps) board
      empty' = (`empty` board) . localToGlobal
      kingPath = castlingKingPath castling
      rookPath = castlingRookPath castling
      king = head kingPath
      rook = head rookPath
      king' = last kingPath
      rook' = last rookPath
  guard (case figureAt board (localToGlobal king) of
    Figure col (King Castleable) | col == color -> True
    _ -> False)
  guard (case figureAt board (localToGlobal rook) of
    Figure col (Rook Castleable) | col == color -> True
    _ -> False)
  guard (all (not . attacked') (kingPath))
  guard (all (empty' <||> (== rook)) (tail kingPath))
  guard (all (empty' <||> (== king)) (tail rookPath))
  board'' <- moveFigure' (localToGlobal king) (localToGlobal king') board
  board' <- moveFigure' (localToGlobal rook) (localToGlobal rook') board''
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
