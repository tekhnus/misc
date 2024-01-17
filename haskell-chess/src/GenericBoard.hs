module GenericBoard
  ( GenericBoard
  , aGenericBoard
  , Position
  , allPositions
  , figureAt'
  , putFigure
  , mapSquares
  , showPos
  ) where

data GenericBoard a =
  GenericBoard [[a]]

aGenericBoard :: [[a]] -> GenericBoard a
aGenericBoard = GenericBoard

instance Show a => Show (GenericBoard a) where
  show (GenericBoard []) = ""
  show (GenericBoard [row]) = concat (map show row)
  show (GenericBoard (row:rows)) =
    (show (GenericBoard [row])) ++ "\n" ++ (show (GenericBoard rows))

north :: Int -> GenericBoard a -> GenericBoard a
north n (GenericBoard rows) = GenericBoard (take n rows)

south :: Int -> GenericBoard a -> GenericBoard a
south n (GenericBoard rows) = GenericBoard (drop n rows)

vMerge :: GenericBoard a -> GenericBoard a -> GenericBoard a
vMerge (GenericBoard upperRows) (GenericBoard lowerRows) =
  GenericBoard (upperRows ++ lowerRows)

west :: Int -> GenericBoard a -> GenericBoard a
west n (GenericBoard rows) = GenericBoard (map (take n) rows)

east :: Int -> GenericBoard a -> GenericBoard a
east n (GenericBoard rows) = GenericBoard (map (drop n) rows)

hMerge :: GenericBoard a -> GenericBoard a -> GenericBoard a
hMerge (GenericBoard leftRows) (GenericBoard rightRows) =
  GenericBoard (zipWith (++) leftRows rightRows)

type Position = (Int, Int)

showPos :: Position -> String
showPos (row, col) = [("abcdefgh" !! col), "12345678" !! row]

allPositions :: [Position]
allPositions = [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]

figureAt' :: GenericBoard a -> Position -> a
figureAt' (GenericBoard rows) (row, column) = (rows !! row) !! column

putFigure :: Position -> a -> GenericBoard a -> GenericBoard a
putFigure (0, 0) figure (GenericBoard ((_:row):rows)) =
  GenericBoard ((figure : row) : rows)
putFigure (0, column) figure board =
  hMerge (west column board) (putFigure (0, 0) figure (east column board))
putFigure (row, column) figure board =
  vMerge (north row board) (putFigure (0, column) figure (south row board))

mapSquares :: (a -> a) -> GenericBoard a -> GenericBoard a
mapSquares upd (GenericBoard squares) = GenericBoard (map (map upd) squares)
