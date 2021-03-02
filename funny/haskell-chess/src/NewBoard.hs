module NewBoard
  ( newBoard
  ) where

import Board

newBoard :: Board
newBoard =
  aBoard
    [ map (Figure White) (initialRank)
    , rankOf (Figure White Pawn)
    , rankOf Empty
    , rankOf Empty
    , rankOf Empty
    , rankOf Empty
    , rankOf (Figure Black Pawn)
    , map (Figure Black) initialRank
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
