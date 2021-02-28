import Text.Printf
import Rules

perft :: Int -> [State]
perft 0 = [newGame]
perft n = concatMap followingStates (perft (n - 1))
  where followingStates s = map snd (basicMoves s)

main :: IO ()
main = do
    mapM_ (\n -> printf "%d %d\n" n (length (perft n))) [1..3]
