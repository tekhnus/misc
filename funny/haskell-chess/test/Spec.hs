import Text.Printf
import Rules
import FEN

perft :: State -> Int -> [State]
perft s 0 = [s]
perft s n = concatMap followingStates (perft s (n - 1))
  where followingStates s' = map snd (moves s')

main :: IO ()
main = do
    mapM_ (\n -> printf "%d %d\n\n" n (length (perft newGame n))) [1..3]
    let (Just position5) = readState "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
    print position5
    mapM_ (\n -> printf "%d %d\n\n" n (length (perft position5 n))) [1..3]
    let (Just position2) = readState "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"
    print position2
    mapM_ (\n -> printf "%d %d\n\n" n (length (perft position2 n))) [1..3]
    let (Just myposition) = readState "rnbqkbnr/pp11pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6"
    print myposition
    mapM_ (\n -> printf "%d %d\n\n" n (length (perft myposition n))) [1]
