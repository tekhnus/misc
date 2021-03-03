{-# LANGUAGE LambdaCase #-}

module Rules
  ( State(..)
  , makeMove
  , newGame
  -- for debug
  , basicMoves, basicMovesFromPosition, positionsAtDirection, makeBasicMove, moveFigure', moves
  ) where

import Board
import Control.Conditional
import Control.Monad ((<=<))
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

type BasicMove = (Position, Position, Maybe Piece)

data Castling =
  KingsideCastling | QueensideCastling
  deriving (Eq, Read)

type Move = Either BasicMove Castling

instance Show State where
  show (State _ board) = show board

applyPromotion :: Color -> Position -> Maybe Piece -> Board -> Maybe Board
applyPromotion _ _ Nothing = return
applyPromotion col pos (Just fig) = emplaceFigure pos (Figure col fig)

compose :: Monad m => [a -> m a] -> a -> m a
compose = foldr (<=<) return

clearEnPassant :: Board -> Maybe Board
clearEnPassant b =
    let cleaners = map makeCleaner allPositions
        cleanAll = compose cleaners
        makeCleaner pos = replaceFigure pos cleanEnPassant
        cleanEnPassant EnPassant = Empty
        cleanEnPassant x = x
     in cleanAll b

manageEnPassant :: BasicMove -> Board -> Maybe Board
manageEnPassant ((fromRow, fromCol), toPosition@(toRow, _), _) b =
    -- TODO: delete old enpassant
    let Figure _ piece = figureAt b toPosition
        diff = abs (fromRow - toRow)
        putEnPassant pos' b' = emplaceFigure pos' EnPassant b'
     in case piece of
        Pawn | diff == 2 -> putEnPassant ((fromRow + toRow) `div` 2, fromCol) b
        _ -> Just b

makeBasicMove :: BasicMove -> State -> Maybe State
makeBasicMove move@(fromPosition, toPosition, promo) (State color board) = do
  board' <- (board & (moveFigure' fromPosition toPosition))
  board'' <- clearEnPassant board'
  board''' <- manageEnPassant move board''
  board'''' <- applyPromotion color toPosition promo board'''
  return (State (opposite color) board'''')

availablePositionsAtDirection :: State -> Position -> Int -> Direction -> [Position]
availablePositionsAtDirection _ _ 0 _ = []
availablePositionsAtDirection (State color board) pos dist dir =
  let
    posList = positionsAtDirection pos dist dir
    empty' p =
      case figureAt board p of
        Figure _ _ -> False
        EnPassant -> True
        Empty -> True
    (empty, rest) = span empty' posList
    possibleTake p =
      case figureAt board p of
        Figure c _ | c /= color -> True
                   | otherwise -> False
        EnPassant -> undefined
        Empty -> undefined
   in empty ++ (filter possibleTake (take 1 rest))

promotions :: State -> Position -> [Maybe Piece]
promotions (State col board) p@(row, _) =
    let fig = figureAt board p
        promRow = case col of White -> 6; Black -> 1
     in case fig of
        (Figure fcol Pawn) | fcol == col && row == promRow -> map Just [Rook NonCastleable, Knight, Bishop, Queen]
        (Figure _ _) -> [Nothing]
        EnPassant -> [Nothing]
        Empty -> [Nothing]

basicMovesFromPosition :: State -> Position -> [(BasicMove, State)]
basicMovesFromPosition state@(State color board) position =
  case (figureAt board position) of
    Figure fcolor piece | fcolor == color ->
      let moveStatePair pos' = do
            promo <- promotions state position
            let m = (position, pos', promo)
            st <- maybeToList (makeBasicMove m state)
            return (m, st)
          simpleMoves dist directions st po = (concatMap (availablePositionsAtDirection st po dist) directions)
          isTakePawn po' = case (figureAt board po') of
            Empty -> False
            EnPassant -> True
            Figure _ _ -> True
          isNonTakePawn po' = case (figureAt board po') of
            Empty -> True
            EnPassant -> True
            Figure _ _ -> False
          isNotMyFigure po' = case (figureAt board po') of
            Figure col' _ | col' == color -> False
                          | otherwise -> True
            Empty -> True
            EnPassant -> True
          deltaMoves deltas _ _ = filter isNotMyFigure (catMaybes (map (`applyDelta` position) deltas))
          pawnMovingDirections = case color of
            White -> [B]
            Black -> [W]
          pawnTakingDirections = case color of
            White -> [BK, BQ]
            Black -> [WK, WQ]
          compose2 = fmap . fmap -- compose a one-arg function with a two-arg function
          liftedConcat f g x y = (f x y) ++ (g x y) -- concat a couple of two-argument list-returning functions
          pawnDist = pawnDistance color position
          pawnDistance White (1, _) = 2
          pawnDistance Black (6, _) = 2
          pawnDistance _ _ = 1
          positionFunction = case piece of
            Rook _ -> simpleMoves 8 [B, W, K, Q]
            Bishop -> simpleMoves 8 [BK, BQ, WK, WQ]
            Queen -> simpleMoves 8 [B, W, K, Q, BK, BQ, WK, WQ]
            King _ -> simpleMoves 1 [B, W, K, Q, BK, BQ, WK, WQ]
            Knight -> deltaMoves [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]
            Pawn -> liftedConcat ((filter isNonTakePawn) `compose2` (simpleMoves pawnDist pawnMovingDirections)) ((filter isTakePawn) `compose2` (simpleMoves 1 pawnTakingDirections))
          positions = positionFunction state position
       in concatMap moveStatePair positions
       | otherwise -> []
    Empty -> []
    EnPassant -> []

basicMoves' :: State -> [(BasicMove, State)]
basicMoves' state = concatMap (basicMovesFromPosition state) allPositions

castlingKingPath :: Castling -> [Int]
castlingKingPath KingsideCastling = [4, 5, 6]
castlingKingPath QueensideCastling = [4, 3, 2]

castlingRookPath :: Castling -> [Int]
castlingRookPath KingsideCastling = [7, 6, 5]
castlingRookPath QueensideCastling = [0, 1, 2, 3]

localToGlobal :: Color -> Int -> Position
localToGlobal White n = (0, n)
localToGlobal Black n = (7, n)

attacked :: Color -> Position -> Board -> Bool
attacked color position board = any attack mvs
  where
    state = State (opposite color) board
    mvs = basicMoves' state
    attack ((_, x', _), _) = x' == position

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

basicMoves :: State -> [(BasicMove, State)]
basicMoves = (filter isValidBasicMove) . basicMoves'
  where isValidBasicMove (_, (State col b)) = not (kingAttacked (opposite col) b)

makeCastling :: Castling -> State -> Maybe State
makeCastling castling (State color board) = do
  let localToGlobal' = localToGlobal color
      attacked' ps = kingAttackedOn color (localToGlobal' ps) board
      empty' = (\case Empty -> True; EnPassant -> True; Figure _ _ -> False) . (figureAt board) . localToGlobal'
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
  castling <- [KingsideCastling, QueensideCastling]
  state' <- maybeToList (makeCastling castling state)
  return (castling, state')

moves :: State -> [(Move, State)]
moves state = (map cbm (basicMoves state)) ++ (map cct (castlings state))
  where cbm (bm, s) = (Left bm, s)
        cct (c, s) = (Right c, s)

makeMove :: Move -> State -> Maybe State
makeMove move state = lookup move (moves state)

newGame :: State
newGame = State White newBoard
