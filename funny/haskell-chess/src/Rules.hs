{-# LANGUAGE LambdaCase #-}

module Rules
  ( State(..)
  , makeMove
  , newGame
  , Move
  -- for debug
  , basicMoves
  , basicMovesFromPosition
  , positionsAtDirection
  , makeBasicMove
  , moveFigure'
  , moves
  ) where

import           Board
import           Control.Applicative (liftA2)
import           Control.Conditional
import           Data.Function
import           Data.List
import           Data.Maybe
import           NewBoard

positionsAtDirection :: Position -> Int -> Direction -> [Position]
positionsAtDirection _ 0 _ = []
positionsAtDirection position dist direction =
  case (moveIn direction position) of
    Nothing -> []
    Just position' ->
      position' : (positionsAtDirection position' (dist - 1) direction)

update :: Square -> Square
update (Figure color (Rook Castleable)) = Figure color (Rook NonCastleable)
update (Figure color (King Castleable)) = Figure color (King NonCastleable)
update figure                           = figure

moveFigure' :: Position -> Position -> Board -> Maybe Board
moveFigure' fromPosition toPosition board = do
  board' <- board & (moveFigure fromPosition toPosition)
  return (replaceFigure toPosition update board')

data State =
  State Color
        Board

type BasicMove = (Position, Position, Maybe Piece)

data Castling
  = KingsideCastling
  | QueensideCastling
  deriving (Eq, Read, Show)

data Move
  = BasicMove BasicMove
  | Castling Castling
  deriving (Eq)

instance Show Move where
  show (BasicMove (pos, pos', _))   = (showPos pos) ++ (showPos pos')
  show (Castling KingsideCastling)  = "KS"
  show (Castling QueensideCastling) = "KS"

instance Show State where
  show (State _ board) = show board

applyPromotion :: Color -> Position -> Maybe Piece -> Board -> Maybe Board
applyPromotion _ _ Nothing        = return
applyPromotion col pos (Just fig) = emplaceFigure pos (Figure col fig)

clearEnPassant :: Board -> Board
clearEnPassant b =
  let cleanEnPassant' EnPassant = Empty
      cleanEnPassant' x         = x
   in replaceAllFigures cleanEnPassant' b

manageEnPassant :: BasicMove -> Board -> Maybe Board
manageEnPassant ((fromRow, fromCol), toPosition@(toRow, _), _) b =
  let diff = abs (fromRow - toRow)
      putEnPassant pos' b' = emplaceFigure pos' EnPassant b'
   in case figureAt b toPosition of
        Figure _ Pawn
          | diff == 2 -> putEnPassant ((fromRow + toRow) `div` 2, fromCol) b
        _ -> Just b

eatEnPassant :: BasicMove -> Board -> Maybe Board
eatEnPassant (fromPos@(fromRow, _), toPos@(_, toCol), _) board =
  case (figureAt board fromPos, figureAt board toPos) of
    (Figure _ Pawn, EnPassant) -> emplaceFigure eatPos Empty board
    _                          -> Just board
  where
    eatPos = (fromRow, toCol)

makeBasicMove :: BasicMove -> State -> Maybe State
makeBasicMove move@(fromPosition, toPosition, promo) (State color board) = do
  board0 <- eatEnPassant move board
  board' <- (board0 & (moveFigure' fromPosition toPosition))
  let board'' = clearEnPassant board'
  board''' <- manageEnPassant move board''
  board'''' <- applyPromotion color toPosition promo board'''
  return (State (opposite color) board'''')

takeWhileNothing :: (a -> Maybe b) -> [a] -> ([a], Maybe (a, b))
takeWhileNothing _ [] = ([], Nothing)
takeWhileNothing f (x:xs) =
  case f x of
    Just y  -> ([], Just (x, y))
    Nothing -> (\(p, r) -> (x : p, r)) (takeWhileNothing f xs)

availablePositionsAtDirection ::
     State -> Position -> Int -> Direction -> [Position]
availablePositionsAtDirection _ _ 0 _ = []
availablePositionsAtDirection (State color board) pos dist dir =
  let posList = positionsAtDirection pos dist dir
      colorof (Figure col _) = Just col
      colorof _              = Nothing
      (empty, mbtake) = takeWhileNothing (colorof . figureAt board) posList
      takePositions =
        case mbtake of
          Just (takepos, fcolor)
            | fcolor /= color -> [takepos]
          _ -> []
   in empty ++ takePositions

promotions :: State -> Position -> [Maybe Piece]
promotions (State col board) p@(row, _) =
  let fig = figureAt board p
      promRow =
        case col of
          White -> 6
          Black -> 1
   in case fig of
        (Figure fcol Pawn)
          | fcol == col && row == promRow ->
            map Just [Rook NonCastleable, Knight, Bishop, Queen]
        (Figure _ _) -> [Nothing]
        EnPassant -> [Nothing]
        Empty -> [Nothing]

pawnMovingDistance :: (State, Position) -> Int
pawnMovingDistance ((State White _), (1, _)) = 2
pawnMovingDistance ((State Black _), (6, _)) = 2
pawnMovingDistance _                         = 1

simpleMoves :: Int -> [Direction] -> State -> Position -> [Position]
simpleMoves dist directions st po =
  availablePositionsAtDirection st po dist =<< directions

isNotMyFigure :: State -> Position -> Bool
isNotMyFigure (State col bo) po' =
  case (figureAt bo po') of
    Figure col' _
      | col' == col -> False
    _ -> True

deltaMoves :: [(Int, Int)] -> State -> Position -> [Position]
deltaMoves deltas st po =
  filter (isNotMyFigure st) (catMaybes (map (`applyDelta` po) deltas))

simpleMoves' :: Int -> [Direction] -> (State, Position) -> [Position]
simpleMoves' a b (c, d) = simpleMoves a b c d

isTakePawn :: (State, b) -> Position -> Bool
isTakePawn ((State _ bo), _) po' = isTakePawn' (figureAt bo po')

isTakePawn' :: Square -> Bool
isTakePawn' Empty        = False
isTakePawn' EnPassant    = True
isTakePawn' (Figure _ _) = True

isNonTakePawn :: (State, b) -> Position -> Bool
isNonTakePawn ((State _ bo), _) po' = isNonTakePawn' (figureAt bo po')

isNonTakePawn' :: Square -> Bool
isNonTakePawn' Empty        = True
isNonTakePawn' EnPassant    = True
isNonTakePawn' (Figure _ _) = False

pawnMovingDirections :: (State, b) -> [Direction]
pawnMovingDirections (State col _, _) =
  case col of
    White -> [B]
    Black -> [W]

pawnTakingDirections :: (State, b) -> [Direction]
pawnTakingDirections (State col _, _) =
  case col of
    White -> [BK, BQ]
    Black -> [WK, WQ]

pawnMoves :: (State, Position) -> [Position]
pawnMoves =
  let mbMoves = do
        d <- pawnMovingDistance
        dirs <- pawnMovingDirections
        simpleMoves' d dirs
      pawnMvs' = filter <$> isNonTakePawn <*> mbMoves
      pawnTakes =
        filter <$> isTakePawn <*> (simpleMoves' 1 =<< pawnTakingDirections)
      positionFunction = liftA2 (++) pawnMvs' pawnTakes
   in positionFunction

pawnMoves' :: State -> Position -> [Position]
pawnMoves' a b = pawnMoves (a, b)

getPositionFunction :: Piece -> State -> Position -> [Position]
getPositionFunction (Rook _) = simpleMoves 8 [B, W, K, Q]
getPositionFunction Bishop = simpleMoves 8 [BK, BQ, WK, WQ]
getPositionFunction Queen = simpleMoves 8 [B, W, K, Q, BK, BQ, WK, WQ]
getPositionFunction (King _) = simpleMoves 1 [B, W, K, Q, BK, BQ, WK, WQ]
getPositionFunction Knight =
  deltaMoves
    [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]
getPositionFunction Pawn = pawnMoves'

basicMovesOfPiece :: Piece -> State -> Position -> [BasicMove]
basicMovesOfPiece piece state position =
  let moveStatePair pos' = do
        promo <- promotions state position
        let m = (position, pos', promo)
        return m
      positions = getPositionFunction piece state position
   in moveStatePair =<< positions

basicMovesFromPosition :: State -> Position -> [BasicMove]
basicMovesFromPosition state@(State color board) position =
  case (figureAt board position) of
    Figure fcolor piece
      | fcolor == color -> basicMovesOfPiece piece state position
    _ -> []

basicMoves' :: State -> [BasicMove]
basicMoves' state = concatMap (basicMovesFromPosition state) allPositions

castlingKingAndRook :: State -> Castling -> Maybe (Int, Int)
castlingKingAndRook (State color board) castling = do
  let isNiceKing (Figure fcol (King Castleable))
        | fcol == color = True
      isNiceKing _ = False
      isNiceRook (Figure fcol (Rook Castleable))
        | fcol == color = True
      isNiceRook _ = False
      homeRow = map (localToGlobal color) [0 .. 7]
      kingPos = find (isNiceKing . figureAt board) homeRow
  (_, kingColumn) <- kingPos
  let endColumn =
        case castling of
          KingsideCastling  -> 7
          QueensideCastling -> 0
      searchedPositions =
        map (localToGlobal color) (numbersBetween kingColumn endColumn)
  (_, rookColumn) <- find (isNiceRook . figureAt board) searchedPositions
  return (kingColumn, rookColumn)

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
kingAttacked color board = attacked color (theKing color board) board

basicMoves :: State -> [(BasicMove, State)]
basicMoves s =
  ((filter isValidBasicMove) . mapMaybe computeState . basicMoves') s
  where
    isValidBasicMove (_, (State col b)) = not (kingAttacked (opposite col) b)
    computeState m = do
      s' <- makeBasicMove m s
      return (m, s')

numbersBetween :: Int -> Int -> [Int]
numbersBetween m n
  | m <= n = [m .. n]
  | otherwise = reverse [n .. m]

makeCastling :: Castling -> State -> Maybe State
makeCastling castling state@(State color board) = do
  (king, rook) <- castlingKingAndRook state castling
  let localToGlobal' = localToGlobal color
      attacked' ps = kingAttackedOn color (localToGlobal' ps) board
      empty' =
        (\case
           Empty -> True
           EnPassant -> True
           Figure _ _ -> False) .
        (figureAt board) . localToGlobal'
      king' =
        case castling of
          KingsideCastling  -> 6
          QueensideCastling -> 2
      kingPath = numbersBetween king king'
      rook' =
        case castling of
          KingsideCastling  -> 5
          QueensideCastling -> 3
      rookPath = numbersBetween rook rook'
  guard (all (not . attacked') (kingPath))
  guard (all (empty' <||> (== rook)) (tail kingPath))
  guard (all (empty' <||> (== king)) (tail rookPath))
  let board''' = clearEnPassant board
  board0 <- emplaceFigure (localToGlobal' king) Empty board'''
  board1 <- emplaceFigure (localToGlobal' rook) Empty board0
  board2 <-
    emplaceFigure
      (localToGlobal' king')
      (Figure color (King NonCastleable))
      board1
  board' <-
    emplaceFigure
      (localToGlobal' rook')
      (Figure color (Rook NonCastleable))
      board2
  return (State (opposite color) board')

castlings :: State -> [(Castling, State)]
castlings state = do
  castling <- [KingsideCastling, QueensideCastling]
  state' <- maybeToList (makeCastling castling state)
  return (castling, state')

moves :: State -> [(Move, State)]
moves state = (map cbm (basicMoves state)) ++ (map cct (castlings state))
  where
    cbm (bm, s) = (BasicMove bm, s)
    cct (c, s) = (Castling c, s)

makeMove :: Move -> State -> Maybe State
makeMove move state = lookup move (moves state)

newGame :: State
newGame = State White newBoard
