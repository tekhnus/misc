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
  return (replaceFigure toPosition update board')

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

clearEnPassant :: Board -> Board
clearEnPassant b =
    let cleanEnPassant EnPassant = Empty
        cleanEnPassant x = x
        cleanPos pos board = replaceFigure pos cleanEnPassant board
     in foldr cleanPos b allPositions

manageEnPassant :: BasicMove -> Board -> Maybe Board
manageEnPassant ((fromRow, fromCol), toPosition@(toRow, _), _) b =
    let Figure _ piece = figureAt b toPosition
        diff = abs (fromRow - toRow)
        putEnPassant pos' b' = emplaceFigure pos' EnPassant b'
     in case piece of
        Pawn | diff == 2 -> putEnPassant ((fromRow + toRow) `div` 2, fromCol) b
        _ -> Just b

eatEnPassant :: BasicMove -> Board -> Maybe Board
eatEnPassant (fromPos@(fromRow, _), toPos@(_, toCol), _) board = 
    case (figureAt board fromPos, figureAt board toPos) of
        (Figure _ Pawn, EnPassant) -> emplaceFigure eatPos Empty board
        _ -> Just board
  where eatPos = (fromRow, toCol)

makeBasicMove :: BasicMove -> State -> Maybe State
makeBasicMove move@(fromPosition, toPosition, promo) (State color board) = do
  board0 <- eatEnPassant move board
  board' <- (board0 & (moveFigure' fromPosition toPosition))
  let board'' = clearEnPassant board'
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

basicMovesFromPosition :: State -> Position -> [BasicMove]
basicMovesFromPosition state@(State color board) position =
  case (figureAt board position) of
    Figure fcolor piece | fcolor == color ->
      let moveStatePair pos' = do
            promo <- promotions state position
            let m = (position, pos', promo)
            return m
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

basicMoves' :: State -> [BasicMove]
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
    attack ((_, x', _)) = x' == position

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
basicMoves s = ((filter isValidBasicMove) . mapMaybe computeState . basicMoves') s
  where isValidBasicMove (_, (State col b)) = not (kingAttacked (opposite col) b)
        computeState m = do
          s' <- makeBasicMove m s
          return (m, s')

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
