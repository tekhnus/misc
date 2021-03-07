import           FEN
import           Rules
import           System.Environment
import           Text.Printf

perft :: State -> Int -> [State]
perft s 0 = [s]
perft s n = concatMap followingStates (perft s (n - 1))
  where
    followingStates s' = map snd (moves s')

detailedPerft :: State -> Int -> [(Move, Int)]
detailedPerft s n = map (fmap (\s' -> length (perft s' (n - 1)))) (moves s)

main :: IO ()
main = do
  [depthS, fen] <- getArgs
  let depth = read depthS
  Just state <- return (readState fen)
  mapM_ (\(m, n) -> printf "%s: %d\n" (show m) n) (detailedPerft state depth)
