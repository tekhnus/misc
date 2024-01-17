module Matching where

-- Проверить соответствие имени простому шаблону без звёздочек
matchesSimple :: String -> FilePath -> Bool
matchesSimple "" "" = True
matchesSimple (x:pattern) (y:path)
    | x == '?' || x == y = matchesSimple pattern path
matchesSimple _ _ = False

-- Проверить соответствие префикса имени простому шаблону
isSimplePrefixOf :: String -> FilePath -> Bool
isSimplePrefixOf pattern path = matchesSimple pattern (take (length pattern) path)

-- Проверить соответствие суффикса имени простому шаблону
isSimpleSuffixOf :: String -> FilePath -> Bool
isSimpleSuffixOf pattern path = isSimplePrefixOf (reverse pattern) (reverse path)

-- Разбить строку на части звёздочками
-- splitByStars "This*is*a**text*" == ["This", "is", "a", "", "text", ""]
-- Никогда не возвращает пустой список: splitByStars "" == [""]
splitByStars :: String -> [String]
splitByStars = foldr updateTokens [""] 
    where updateTokens '*' tokens = "" : tokens
          updateTokens c tokens = (c : head tokens) : tail tokens

-- Проверить, можно ли в имени последовательно без пересечений выделить заданные подстроки,
-- возможно начиная не с начала имени, но заканчивая в конце
-- hasSubsequence ["lo", "Ha", "kel"] "I love Haskell!" == True
--                                       ^^   ^^ ^^^ 
hasSubsequence :: [String] -> FilePath -> Bool
hasSubsequence [substr] path = substr `isSimpleSuffixOf` path
hasSubsequence ("":substrs) "" = hasSubsequence substrs ""
hasSubsequence _ "" = False
hasSubsequence substrs@(substr:rest) path
    | substr `isSimplePrefixOf` path = hasSubsequence rest (drop (length substr) path)
    | otherwise = hasSubsequence substrs (tail path)

-- Проверить, можно ли в имени последовательно без пересечений выделить заданные подстроки,
-- начиная с начала и заканчивая в конце строки
-- hasSubsequence ["I", "ve", "ell!"] "I love Haskell!" == True
--                                     ^   ^^     ^^^^
hasSubsequenceFull :: [String] -> FilePath -> Bool
hasSubsequenceFull [substr] path = matchesSimple substr path
hasSubsequenceFull substrs path
    = (head substrs `isSimplePrefixOf` path) && hasSubsequence substrs path

-- Проверить соответствие имени шаблону
matches :: String -> FilePath -> Bool
matches pattern = hasSubsequenceFull (splitByStars pattern)
