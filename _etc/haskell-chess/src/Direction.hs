module Direction
  ( Direction(..)
  , applyDelta
  , moveIn
  ) where

import           GenericBoard

data Direction
  = B
  | W
  | K
  | Q
  | BK
  | BQ
  | WK
  | WQ
  deriving (Show) -- for debug

delta :: Direction -> (Int, Int)
delta B  = (1, 0)
delta W  = (-1, 0)
delta K  = (0, 1)
delta Q  = (0, -1)
delta BK = (1, 1)
delta BQ = (1, -1)
delta WK = (-1, 1)
delta WQ = (-1, -1)

applyDelta :: (Int, Int) -> Position -> Maybe Position
applyDelta (da, dn) (a, n)
  | a' < 0 || a' >= 8 || n' < 0 || n' >= 8 = Nothing
  | otherwise = Just (a', n')
  where
    (a', n') = (a + da, n + dn)

moveIn :: Direction -> Position -> Maybe Position
moveIn direction = applyDelta (delta direction)
