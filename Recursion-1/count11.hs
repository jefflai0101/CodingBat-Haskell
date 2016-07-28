--Given a string, compute recursively (no loops) the number of "11" substrings in the string.
--The "11" substrings should not overlap.

--count11("11abc11")     → 2
--count11("abc11x11x11") → 3
--count11("111")         → 1

count11 :: [Char] -> Int
count11 (x:y:xs)
     | x == '1' && y == '1' = 1 + count11 xs
     | otherwise            = count11 (y:xs)
count11 x                   = 0