module NewBoard
  ( newBoard
  ) where

import Board

newBoard :: Board
newBoard =
  aBoard
    [ map (Figure Black) (initialRank)
    , rankOf (Figure Black (Pawn Stable))
    , rankOf Empty
    , rankOf Empty
    , rankOf Empty
    , rankOf Empty
    , rankOf (Figure White (Pawn Stable))
    , map (Figure White) initialRank
    ]
  where
    initialRank =
      [ (Rook Castleable)
      , Knight
      , Bishop
      , Queen
      , (King Castleable)
      , Bishop
      , Knight
      , (Rook Castleable)
      ]
    rankOf = replicate 8
