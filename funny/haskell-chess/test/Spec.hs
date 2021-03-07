import           FEN
import           Rules
import           Test.Hspec

perft :: State -> Int -> [State]
perft s 0 = [s]
perft s n = concatMap followingStates (perft s (n - 1))
  where
    followingStates s' = map snd (moves s')

perftLengths :: String -> [Int] -> [Int]
perftLengths fen depths =
  let ms = readState fen
   in case ms of
        Right s -> map (\n -> (length (perft s n))) depths
        Left _  -> []

main :: IO ()
main =
  hspec $ do
    describe "perft" $ do
      it "handles chess960 position #11 from wiki" $
        perftLengths
          "qnr1bkrb/pppp2pp/3np3/5p2/8/P2P2P1/NPP1PP1P/QN1RBKRB w GDg - 3 9"
          [1 .. 3] `shouldBe`
        [33, 823, 26895]
      it "handles chess960 position #33 from wiki" $
        perftLengths
          "bnn1qrkr/pp1ppp1p/2p5/b3Q1p1/8/5P1P/PPPPP1P1/BNNB1RKR w HFhf - 2 9"
          [1 .. 3] `shouldBe`
        [44, 920, 35830]
      it "handles the initial position" $
        perftLengths
          "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          [1 .. 3] `shouldBe`
        [20, 400, 8902]
      it "handles the position #5 from wiki" $
        perftLengths
          "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
          [1 .. 3] `shouldBe`
        [44, 1486, 62379]
      it "handles the position #2 from wiki" $
        perftLengths
          "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"
          [1 .. 3] `shouldBe`
        [48, 2039, 97862]
      it "handles a position where an immediate enpassant is possible" $
        perftLengths
          "rnbqkbnr/pp11pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6"
          [1 .. 1] `shouldBe`
        [31]
