module FEN
  ( readState
  ) where

import           Control.Monad
import           Data.Char
import           Data.List.Split
import           Data.Maybe

-- import Debug.Trace
import           Board
import           Rules

data FENPiece
  = SPawn
  | SKnight
  | SBishop
  | SRook
  | SQueen
  | SKing
  deriving (Show)

data FENSquare
  = FENPiece Color
             FENPiece
  | FENEmpty Int
  deriving (Show)

data CastlingSide
  = KCastle
  | QCastle
  | SpecialCastle Int
  deriving (Show)

data FENState =
  FENState [[FENSquare]]
           Color
           [(Color, CastlingSide)]
           (Maybe Position)
           Int
           Int
  deriving (Show)

parseWord :: String -> (String, String)
parseWord chars = (w ++ s, r)
  where
    (w, sr) = break (== ' ') chars
    (s, r) = span (== ' ') sr

parseInt :: String -> (Int, String)
parseInt s = (read n, s')
  where
    (n, s') = parseWord s

parseSquares :: String -> ([[FENSquare]], String)
parseSquares s = (parsedBoard, s')
  where
    (board, s') = parseWord s
    boardWoSpace = init board
    rows = splitOn "/" boardWoSpace
    parseSymbol 'P' = FENPiece White SPawn
    parseSymbol 'N' = FENPiece White SKnight
    parseSymbol 'B' = FENPiece White SBishop
    parseSymbol 'R' = FENPiece White SRook
    parseSymbol 'Q' = FENPiece White SQueen
    parseSymbol 'K' = FENPiece White SKing
    parseSymbol 'p' = FENPiece Black SPawn
    parseSymbol 'n' = FENPiece Black SKnight
    parseSymbol 'b' = FENPiece Black SBishop
    parseSymbol 'r' = FENPiece Black SRook
    parseSymbol 'q' = FENPiece Black SQueen
    parseSymbol 'k' = FENPiece Black SKing
    parseSymbol n
      | ('1' <= n) && (n <= '8') = FENEmpty ((ord n) - (ord '1') + 1)
    parseSymbol _ = undefined
    parseRow r = map parseSymbol r
    parsedBoard = map parseRow rows

parseColor :: String -> (Color, String)
parseColor s = (toColor c, s')
  where
    (c, s') = parseWord s
    toColor ('b':_) = Black
    toColor ('w':_) = White
    toColor _       = undefined

parseCastling :: String -> ([(Color, CastlingSide)], String)
parseCastling s = (mapMaybe toCastlingInfo cas, s')
  where
    (cas, s') = parseWord s
    toCastlingInfo '-' = Nothing
    toCastlingInfo ' ' = Nothing
    toCastlingInfo c   = Just (castlingColor c, castlingType c)
    castlingColor c =
      case isUpper c of
        True -> White
        _    -> Black
    castlingType c =
      case toUpper c of
        'Q' -> QCastle
        'K' -> KCastle
        u
          | u `elem` ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] ->
            SpecialCastle (ord u - ord 'A')
        _ -> undefined

parseEnPassant :: String -> (Maybe Position, String)
parseEnPassant s = (toPosition p, s')
  where
    (p, s') = parseWord s
    toPosition ('-':_) = Nothing
    toPosition (col:(row:_)) =
      Just ((ord row) - (ord '1'), (ord col) - (ord 'a'))
    toPosition _ = undefined

com :: (s -> (a, s)) -> (s -> (b, s)) -> s -> ((a, b), s)
com p1 p2 s =
  let (theA, s') = p1 s
      (theB, s'') = p2 s'
   in ((theA, theB), s'')

parseFENState :: String -> (FENState, String)
parseFENState =
  pack .
  (parseSquares `com` parseColor `com` parseCastling `com` parseEnPassant `com`
   parseInt `com`
   parseInt)
  where
    pack ((((((sq, col), cast), enp), halfc), fullc), s) =
      ((FENState sq col cast enp halfc fullc), s)

readFENState :: String -> FENState
readFENState = fst . parseFENState

fenSquaresToBoard :: [[FENSquare]] -> Board
fenSquaresToBoard rows = aBoard (map fenRowToBoard (reverse rows))
  where
    fenRowToBoard = concatMap fenSquareToSquares
    fenSquareToSquares (FENPiece col SPawn) = [Figure col Pawn]
    fenSquareToSquares (FENPiece col SKnight) = [Figure col Knight]
    fenSquareToSquares (FENPiece col SBishop) = [Figure col Bishop]
    fenSquareToSquares (FENPiece col SRook) = [Figure col (Rook NonCastleable)]
    fenSquareToSquares (FENPiece col SQueen) = [Figure col Queen]
    fenSquareToSquares (FENPiece col SKing) = [Figure col (King Castleable)]
    fenSquareToSquares (FENEmpty n) = replicate n Empty

compose :: Monad m => [a -> m a] -> a -> m a
compose = foldr (<=<) return

readState :: String -> Maybe State
readState s =
  let (FENState fenBoard color allowedCastlings enp _ _) = readFENState s
      board = fenSquaresToBoard fenBoard
      markCastling (col, side) b =
        let row =
              case col of
                White -> 0
                Black -> 7
            rookCol =
              case side of
                QCastle              -> 0
                KCastle              -> 7
                SpecialCastle column -> column
            rp = (row, rookCol)
            b'' = (emplaceFigure rp (Figure col (Rook Castleable))) b
         in b''
      markingFunctions = map markCastling allowedCastlings
      markAll = compose markingFunctions
      board' = markAll board
      board'' =
        case enp of
          Just enp' -> board' >>= emplaceFigure enp' EnPassant
          Nothing   -> board'
   in fmap (State color) board''
