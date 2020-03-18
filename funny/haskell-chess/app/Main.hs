import Control.Monad.Loops
import Rules

playerTurn :: State -> IO State
playerTurn state = do
  print state
  turn <- readLn
  case make turn state of
    Just state' -> return state'
    Nothing -> do
      print "Wrong move, try again"
      playerTurn state

main :: IO ()
main = iterateM_ playerTurn newGame
