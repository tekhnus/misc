{-# LANGUAGE LambdaCase #-}

module FEN
  ( readState
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Function
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

co :: Parse a -> (a -> Parse b) -> Parse b
co (Parse f) createG = Parse co'
  where
    co' s =
      case (f s) of
        Left err      -> Left err
        Right (v, s') -> ((createG v) & (\(Parse g) -> g)) s'

yield :: a -> Parse a
yield x = Parse f
  where
    f s = Right (x, s)

yerr :: String -> Parse a
yerr s = Parse f
  where
    f _ = Left s

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
parseWord =
  getState `co` \r ->
    let (w, r') = break (== ' ') (remainingAsString r)
        (s, r'') = span (== ' ') r'
     in putState (remainingFromString r'') `co` \_ -> yield (w ++ s)

mapEither :: (b -> Either a c) -> [Either a b] -> Either a [c]
mapEither f xs =
  let f' = (>>= f)
      ys = map f' xs
   in case (lefts ys) of
        []     -> Right (rights ys)
        badY:_ -> Left badY

yieldIfGood :: Either String a -> Parse a
yieldIfGood (Right x)  = yield x
yieldIfGood (Left err) = yerr err

parseSquares :: Parse [[FENSquare]]
parseSquares =
  parseWord `co`
  (init >>> splitOn "/" >>> map return >>> mapEither parseRow >>> yieldIfGood)
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
    parseRow = map return >>> mapEither parseSymbol

parseCastling :: Parse [(Color, CastlingSide)]
parseCastling = parseWord `co` \word -> (map toCastlingInfo word) & doYield
  where
    doYield couldBeErrors =
      let errors = lefts couldBeErrors
          normies = rights couldBeErrors
       in case errors of
            []    -> yield (catMaybes normies)
            err:_ -> yerr err
    toCastlingInfo '-' = Right Nothing
    toCastlingInfo ' ' = Right Nothing
    toCastlingInfo c =
      castlingType c >>= \ct -> Right (Just (castlingColor c, ct))
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
parseEnPassant = parseWord `co` toPosition
  where
    toPosition ('-':_) = yield Nothing
    toPosition (col:(row:_)) =
      yield (Just ((ord row) - (ord '1'), (ord col) - (ord 'a')))
    toPosition _ = yerr "Bad enpassant info"

parseColor :: Parse Color
parseColor = parseWord `co` toColor
  where
    toColor ('b':_) = yield Black
    toColor ('w':_) = yield White
    toColor _       = yerr "Bad color"

parseInt :: Parse Int
parseInt = parseWord `co` \word -> yield (read word)

parseFENState :: Parse FENState
parseFENState =
  parseSquares `co` \sq ->
    parseColor `co` \col ->
      parseCastling `co` \cast ->
        parseEnPassant `co` \enp ->
          parseInt `co` \halfc ->
            parseInt `co` \fullc -> yield (FENState sq col cast enp halfc fullc)

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
