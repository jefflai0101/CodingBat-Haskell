--Given an array of ints, compute recursively if the array contains a 6.
--We'll use the convention of considering only the part of the array that begins at the given index.
--In this way, a recursive call can pass index+1 to move down the array. The initial call will pass in index as 0.

--array6([1, 6, 4], 0) → true
--array6([1, 4], 0)    → false
--array6([6], 0)       → true

import Data.Bool

array6 :: [Int] -> Bool
array6 (x:xs) = x == 6 || array6 xs
array6 []     = False

--array6 :: [Int] -> Bool
--array6 (x:xs)
--       | x == 6    = True
--       | otherwise = array6 xs
--array6 x           = False

array6' :: [Int] -> Bool
array6' = foldl (\x y -> x || (y == 6) ) False