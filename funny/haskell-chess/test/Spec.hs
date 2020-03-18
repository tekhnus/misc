import Control.Monad.Loops
import Rules
import Board

playerTurn :: State -> IO State
playerTurn state = do
  print state
  print (moves state)
  turn <- readLn
  case make turn state of
    Just state' -> return state'
    Nothing -> do
      print "Wrong move, try again"
      playerTurn state

main :: IO ()
main = do
    let (State _ newBoard) = newGame
    print (moves newGame)
    print (possibleMoves newGame (6, 0))
    print (movesAtDirection newGame (6, 0) 1 B)
    print (makeMove ((6, 0), (5, 0)) newGame)
    print (moveFigure' (6, 0) (5, 0) newBoard)
    print (moveFigure (6, 0) (5, 0) newBoard)
    print (isKing (figureAt newBoard (5, 0)))
    print (figureAt newBoard (5, 0))
    print (positionsAtDirection (6, 0) 1 B)
    print (moveDirections White (Pawn Stable))
    print (distance (Pawn Stable))
    print (touch newGame (6, 0))
