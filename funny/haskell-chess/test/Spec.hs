import Text.Printf
import Rules
import FEN

perft :: State -> Int -> [State]
perft s 0 = [s]
perft s n = concatMap followingStates (perft s (n - 1))
  where followingStates s' = map snd (basicMoves s')

main :: IO ()
main = do
    mapM_ (\n -> printf "%d %d\n\n" n (length (perft newGame n))) [1..3]
    let position5 = readState "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
    print position5
    mapM_ (\n -> printf "%d %d\n\n" n (length (perft position5 n))) [1..3]
