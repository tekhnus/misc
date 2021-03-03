-- TODO: implement enpassant reading
module FEN
  ( readState
  ) where

import Data.Maybe
import Data.Char
import Data.List.Split
import Control.Monad
-- import Debug.Trace
import Board
import Rules

data FENPiece = SPawn | SKnight | SBishop | SRook | SQueen | SKing
  deriving (Show)
data FENSquare = FENPiece Color FENPiece | FENEmpty Int
  deriving (Show)
data CastlingSide = KCastle | QCastle
  deriving (Show)
data FENState = FENState [[FENSquare]] Color [(Color, CastlingSide)] (Maybe Position) Int Int
  deriving (Show)

parseWord :: String -> (String, String)
parseWord chars = (w ++ s, r)
    where (w, sr) = break (== ' ') chars
          (s, r) = span (== ' ') sr

parseInt :: String -> (Int, String)
parseInt s = (read n, s')
    where (n, s') = parseWord s

parseSquares :: String -> ([[FENSquare]], String)
parseSquares s = (parsedBoard, s')
    where (board, s') = parseWord s
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
          parseSymbol n | ('1' <= n) && (n <= '8') = FENEmpty ((ord n) - (ord '1') + 1)
          parseSymbol _ = undefined
          parseRow r = map parseSymbol r
          parsedBoard = map parseRow rows

parseColor :: String -> (Color, String)
parseColor s = (toColor c, s')
    where (c, s') = parseWord s
          toColor ('b':_) = Black
          toColor ('w':_) = White
          toColor _ = undefined

parseCastling :: String -> ([(Color, CastlingSide)], String)
parseCastling s = (mapMaybe toCastlingInfo cas, s')
    where (cas, s') = parseWord s
          toCastlingInfo 'Q' = Just (White, QCastle)
          toCastlingInfo 'K' = Just (White, KCastle)
          toCastlingInfo 'q' = Just (Black, QCastle)
          toCastlingInfo 'k' = Just (Black, KCastle)
          toCastlingInfo '-' = Nothing
          toCastlingInfo ' ' = Nothing
          toCastlingInfo _ = undefined

parseEnPassant :: String -> (Maybe Position, String)
-- parseEnPassant s | trace ("enpassant " ++ s) False = undefined
parseEnPassant s = (toPosition p, s')
    where (p, s') = parseWord s
          toPosition "- " = Nothing
          toPosition [col, row, ' '] = Just ((ord row) - (ord '1'), (ord col) - (ord 'a'))
          toPosition _ = undefined

com :: (String -> (a, String)) -> (String -> (b, String)) -> String -> ((a, b), String)
com p1 p2 s =
  let (theA, s') = p1 s
      (theB, s'') = p2 s'
   in ((theA, theB), s'')

parseFENState :: String -> (FENState, String)
parseFENState = pack . (parseSquares `com` parseColor `com` parseCastling `com` parseEnPassant `com` parseInt `com` parseInt)
  where pack ((((((sq, col), cast), enp), halfc), fullc), s) = ((FENState sq col cast enp halfc fullc), s)

readFENState :: String -> FENState
readFENState = fst . parseFENState

fenSquaresToBoard :: [[FENSquare]] -> Board
fenSquaresToBoard rows = aBoard (map fenRowToBoard (reverse rows))
    where fenRowToBoard = concatMap fenSquareToSquares
          fenSquareToSquares (FENPiece col SPawn) = [Figure col Pawn]
          fenSquareToSquares (FENPiece col SKnight) = [Figure col Knight]
          fenSquareToSquares (FENPiece col SBishop) = [Figure col Bishop]
          fenSquareToSquares (FENPiece col SRook) = [Figure col (Rook NonCastleable)]
          fenSquareToSquares (FENPiece col SQueen) = [Figure col Queen]
          fenSquareToSquares (FENPiece col SKing) = [Figure col (King NonCastleable)]
          fenSquareToSquares (FENEmpty n) = replicate n Empty


compose :: Monad m => [a -> m a] -> a -> m a
compose = foldr (<=<) return

readState :: String -> Maybe State
readState s = 
    let (FENState fenBoard color allowedCastlings _ _ _) = readFENState s
        board = fenSquaresToBoard fenBoard
        markCastling (col, side) b =
            let kingPos White = (0, 4)
                kingPos Black = (7, 4)
                rookPos White QCastle = (0, 0)
                rookPos Black QCastle = (7, 0)
                rookPos White KCastle = (0, 7)
                rookPos Black KCastle = (7, 7)
                kp = kingPos col
                rp = rookPos col side
                b' = emplaceFigure kp (Figure col (King Castleable)) b
                b'' = b' >>= (emplaceFigure rp (Figure col (Rook Castleable)))
             in b''
        markingFunctions = map markCastling allowedCastlings
        markAll = compose markingFunctions
        board' = markAll board
     in fmap (State color) board'
