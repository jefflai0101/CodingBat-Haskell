--Given a string, compute recursively the number of times lowercase "hi" appears in the string,
--however do not count "hi" that have an 'x' immedately before them.

--countHi2("ahixhi") → 1
--countHi2("ahibhi") → 2
--countHi2("xhixhi") → 0

countHi2 :: [Char] -> Int
countHi2 (x:y:z:xs)
     | x /= 'x' && y == 'h' && z == 'i' = 1 + countHi2 xs
     | otherwise                        = countHi2 (y:z:xs)
countHi2 _                              = 0
