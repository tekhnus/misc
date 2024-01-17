module Main where

import Matching (matches)
import Control.Exception (assert)

testCase pattern expect path
    | matches pattern path == expect = putStrLn ("OK " ++ pattern ++ " " ++ path) >> return True
    | otherwise = putStrLn ("ERROR " ++ pattern ++ " " ++ path) >> return False

testCases (pattern, trus, fals) = do
    truok <- fmap and (mapM (testCase pattern True) trus)
    falok <- fmap and (mapM (testCase pattern False) fals)
    return (truok && falok)

cases = [
    ("pattern", ["pattern"],
                ["patt", "ern", "patternpattern", ""])
    ,("*a*", ["kojbaca", "cadbra", "ahoy", "uxa"],
             ["brudenberg", "kernel", "", "forest"])
    ,("*", ["anything", ""],
           [])
    ,("?ouse", ["house", "mouse"],
               ["ouse", "rous", "goouse"])
    ,("b?n", ["bun", "ban", "bin"],
             ["bn", "bu", "b", "boon"])
    ,("**", ["also anything", ""],
            [])
    ,("", [""],
          ["nothing more"])
    ,("I*Haskell", ["I love Haskell", "I hate Haskell"],
                   ["I love Haskell!", "Love I Haskell"])
    ,("*suffix", ["This is suffix", "suffix"],
                 ["suffix not it is", "ffix", "suff", ""])
    ,("prefix*", ["prefix", "prefixxxx"],
                 ["prefic", "aprefix", "", "pref", "fix"])
    ,("*?aq", ["bcdvatsaq", "lasbatnarlaq", "raaq"],
              ["basa", "blablabqla"])
    ]

summarize results
    | and results = putStrLn "All tests OK"
    | otherwise = putStrLn "Some tests FAILed"
main = mapM testCases cases >>= summarize

