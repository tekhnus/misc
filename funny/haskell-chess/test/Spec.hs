import Data.List
import Rules

main :: IO ()
main = do
    putStrLn (intercalate "\n--------\n" (map (show . snd) (basicMoves newGame)))
