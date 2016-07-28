--We'll say that a "pair" in a string is two instances of a char separated by a char. So "AxA" the A's make a pair.
--Pair's can overlap, so "AxAxA" contains 3 pairs -- 2 for A and 1 for x.
--Recursively compute the number of pairs in the given string.

--countPairs("axa")  → 1
--countPairs("axax") → 2
--countPairs("axbx") → 1

countPairs :: [Char] -> Int
countPairs (x:xs@(xsh:xst:_))
        | x == xst  = 1 + countPairs xs
        | otherwise = countPairs xs
countPairs _        = 0