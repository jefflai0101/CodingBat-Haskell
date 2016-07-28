--Given a string, compute recursively (no loops) the number of times lowercase "hi" appears in the string.

--countHi("xxhixx")  → 1
--countHi("xhixhix") → 2
--countHi("hi")      → 1

countHi :: [Char] -> Int
countHi (x:y:xs)
      | x == 'h' && y == 'i'  = 1 + countHi (y:xs)
      | otherwise             = countHi (y:xs)
countHi x                     = 0

countHi' :: [Char] -> Int
countHi' wStr = sum $ map (\x -> if ((take 2 $ drop x wStr) == "hi") then 1 else 0) [0..length wStr]