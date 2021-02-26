module Rules
  ( State(..)
  , make
  , newGame
  -- for debug
  , moves, possibleMoves, touch, moveDirections, movesAtDirection, positionsAtDirection, distance, makeMove, moveFigure'
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

type MoveFigure = (Position, Position)

data Castling =
  KingsideCastling
  deriving (Eq, Read)

type Move = Either MoveFigure Castling

instance Show State where
  show (State _ board) = show board

makeMove :: MoveFigure -> State -> Maybe State
makeMove (fromPosition, toPosition) (State color board) = do
  board' <- (board & (moveFigure' fromPosition toPosition))
  return (State (opposite color) board')

movesAtDirection :: State -> Position -> Int -> Direction -> [(MoveFigure, State)]
movesAtDirection state@(State _ board) position dist direction =
  untilStop positions
  where
    positions = positionsAtDirection position dist direction
    untilStop [] = []
    untilStop (p:ps) =
      case (figureAt board p) of
        Empty ->
          case makeMove (position, p) state of
            Just state' -> ((position, p), state') : (untilStop ps)
            Nothing     -> untilStop ps
        _ -> []

takesAtDirection :: State -> Position -> Int -> Direction -> [(MoveFigure, State)]
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
            then case makeMove (position, p) state of
                   Just state' -> [((position, p), state')]
                   Nothing     -> []
            else []

touch :: State -> Position -> Maybe Piece
touch (State color board) position =
  case (figureAt board position) of
    Figure color' piece
      | color' == color -> Just piece
    _ -> Nothing

possibleMoves :: State -> Position -> [(MoveFigure, State)]
possibleMoves state@(State color _) position =
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

moves :: State -> [(MoveFigure, State)]
moves state = concatMap (possibleMoves state) allPositions

homeRow :: Color -> Board -> [(Square, Int)]
homeRow _ _ = []

freshKing :: State -> Maybe Int
freshKing (State color board) =
  listToMaybe
    [ position
    | (Figure fcolor (King Castleable), position) <- (homeRow color board)
    , fcolor == color
    ]

castlingKingDst :: Castling -> Int
castlingKingDst KingsideCastling = 5

castlingRookDst :: Castling -> Int
castlingRookDst KingsideCastling = 4

atHomeRow :: Color -> Int -> Position
atHomeRow _ _ = (0, 0)

straightPath :: Int -> Int -> [Int]
straightPath _ _ = []

freshRook :: Castling -> State -> Maybe Int
freshRook _ _ = Nothing

empty :: Position -> Board -> Bool
empty position board =
  case figureAt board position of
    Empty -> True
    _     -> False

attacked :: Color -> Position -> Board -> Bool
attacked color position board = any attack mvs
  where
    state = State (opposite color) board
    mvs = moves state
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
makeCastling castling state@(State color board) = do
  king <- freshKing state
  rook <- freshRook castling state
  let pos = atHomeRow color
      attacked' ps = kingAttackedOn color (pos ps) board
      empty' ps = empty (pos ps) board
      king' = castlingKingDst castling
      rook' = castlingRookDst castling
      kingPath = straightPath king king'
      rookPath = straightPath rook rook'
  board'' <- moveFigure' (pos king) (pos king') board
  board' <- moveFigure' (pos rook) (pos rook') board''
  guard (all (not . attacked') (king : kingPath))
  guard (all (empty' <||> (== rook)) (king' : kingPath))
  guard (all (empty' <||> (== king)) (rook' : rookPath))
  return (State (opposite color) board')

castlings :: State -> [(Castling, State)]
castlings state = do
  castling <- [KingsideCastling]
  state' <- maybeToList (makeCastling castling state)
  return (castling, state')


make :: Move -> State -> Maybe State
make turn state@(State color _) = do
  state'@(State _ board') <-
    case turn of
      Left move       -> lookup move (moves state)
      Right castling -> lookup castling (castlings state)
  (kingAttacked color board') |> state'

newGame :: State
newGame = State White newBoard
