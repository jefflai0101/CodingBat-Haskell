--Given a string and a non-empty substring sub, compute recursively if at least n copies of sub appear in the string somewhere, possibly with overlapping. N will be non-negative.

--strCopies("catcowcat", "cat", 2) → true
--strCopies("catcowcat", "cow", 2) → false
--strCopies("catcowcat", "cow", 1) → true

import Data.Bool

strCopies :: [Char] -> [Char] -> Int -> Bool
strCopies sChar tSub atLeast = atLeast <= (sum $ map (\x -> if ((take 3 $ drop x sChar) == tSub) then 1 else 0) [0..length sChar])
--strCopies sChar tSub atLeast = atLeast <= (sum . map (\x -> if (take 3 x == tSub) then 1 else 0) $ map (\x -> drop x sChar) [0..length sChar])