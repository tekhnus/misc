{-# LANGUAGE LambdaCase #-}

module FEN
  ( readState
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List.Split
import           Data.Maybe

-- import Debug.Trace
import           Board
import           Rules

newtype Remaining =
  Remaining String

newtype Parse a =
  Parse (Remaining -> Either String (a, Remaining))

parse :: Parse a -> Remaining -> Either String a
parse (Parse f) s =
  case f s of
    Left err     -> Left err
    Right (v, _) -> Right v

throw :: String -> Parse a
throw s = Parse f
  where
    f _ = Left s

instance Functor Parse where
  fmap = liftM

instance Applicative Parse where
  pure = return
  (<*>) = ap

instance Monad Parse where
  return x = Parse (\s -> Right (x, s))
  Parse f >>= createG = Parse co'
    where
      co' s =
        case (f s) of
          Left err      -> Left err
          Right (v, s') -> ((createG v) & (\(Parse g) -> g)) s'

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

getState :: Parse Remaining
getState = Parse (\s -> Right (s, s))

putState :: Remaining -> Parse ()
putState s = Parse (\_ -> Right ((), s))

remainingFromString :: String -> Remaining
remainingFromString = Remaining

remainingAsString :: Remaining -> String
remainingAsString (Remaining s) = s

parseWord :: Parse String
parseWord = do
  r <- getState
  let (w, r') = break (== ' ') (remainingAsString r)
      (s, r'') = span (== ' ') r'
  putState (remainingFromString r'')
  return (w ++ s)

returnOrThrow :: Either String a -> Parse a
returnOrThrow (Right x)  = return x
returnOrThrow (Left err) = throw err

parseSquares :: Parse [[FENSquare]]
parseSquares =
  parseWord >>=
  (init >>>
   splitOn "/" >>>
   map (map parseSymbol >>> sequence) >>> sequence >>> returnOrThrow)
  where
    parseSymbol 'P' = Right (FENPiece White SPawn)
    parseSymbol 'N' = Right (FENPiece White SKnight)
    parseSymbol 'B' = Right (FENPiece White SBishop)
    parseSymbol 'R' = Right (FENPiece White SRook)
    parseSymbol 'Q' = Right (FENPiece White SQueen)
    parseSymbol 'K' = Right (FENPiece White SKing)
    parseSymbol 'p' = Right (FENPiece Black SPawn)
    parseSymbol 'n' = Right (FENPiece Black SKnight)
    parseSymbol 'b' = Right (FENPiece Black SBishop)
    parseSymbol 'r' = Right (FENPiece Black SRook)
    parseSymbol 'q' = Right (FENPiece Black SQueen)
    parseSymbol 'k' = Right (FENPiece Black SKing)
    parseSymbol n
      | ('1' <= n) && (n <= '8') = Right (FENEmpty ((ord n) - (ord '1') + 1))
    parseSymbol _ = Left "Bad square"

parseCastling :: Parse [(Color, CastlingSide)]
parseCastling = do
  word <- parseWord
  let castlings = word & map toCastlingInfo & sequence <&> catMaybes
  returnOrThrow castlings
  where
    toCastlingInfo '-' = Right Nothing
    toCastlingInfo ' ' = Right Nothing
    toCastlingInfo c = do
      ct <- castlingType c
      return (Just (castlingColor c, ct))
    castlingColor c =
      case isUpper c of
        True -> White
        _    -> Black
    castlingType c =
      case toUpper c of
        'Q' -> Right QCastle
        'K' -> Right KCastle
        u
          | u `elem` ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'] ->
            Right (SpecialCastle (ord u - ord 'A'))
        _ -> Left "Bad castling flag"

parseEnPassant :: Parse (Maybe Position)
parseEnPassant = parseWord >>= toPosition
  where
    toPosition ('-':_) = return Nothing
    toPosition (col:(row:_)) =
      return (Just ((ord row) - (ord '1'), (ord col) - (ord 'a')))
    toPosition _ = throw "Bad enpassant info"

parseColor :: Parse Color
parseColor = parseWord >>= toColor
  where
    toColor ('b':_) = return Black
    toColor ('w':_) = return White
    toColor _       = throw "Bad color"

parseInt :: Parse Int
parseInt = parseWord >>= (read >>> return)

parseFENState :: Parse FENState
parseFENState = do
  sq <- parseSquares
  col <- parseColor
  cast <- parseCastling
  enp <- parseEnPassant
  halfc <- parseInt
  fullc <- parseInt
  return (FENState sq col cast enp halfc fullc)

readFENState :: String -> Either String FENState
readFENState = parse parseFENState . remainingFromString

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

readState :: String -> Either String State
readState s = readFENState s >>= convertState
  where
    convertState (FENState fenBoard color allowedCastlings enp _ _) =
      let board = fenSquaresToBoard fenBoard
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
          board''' =
            board'' &
            (\case
               Nothing -> Left "parse error :("
               Just bo -> Right bo)
       in fmap (State color) board'''
