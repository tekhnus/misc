import Rules
import Board

main :: IO ()
main = do
    let (State _ newBoard) = newGame
    print (basicMoves newGame)
    print (basicMovesFromPosition newGame (6, 0))
    print (makeBasicMove ((6, 0), (5, 0)) newGame)
    print (moveFigure' (6, 0) (5, 0) newBoard)
    print (moveFigure (6, 0) (5, 0) newBoard)
    print (isKing (figureAt newBoard (5, 0)))
    print (figureAt newBoard (5, 0))
    print (positionsAtDirection (6, 0) 1 B)
    print (moveDirections White (Pawn Stable))
    print (distance (Pawn Stable))
    print (touch newGame (6, 0))
