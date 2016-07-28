--Given an array of ints, is it possible to choose a group of some of the ints,
--such that the group sums to the given target with these additional constraints: 
--all multiples of 5 in the array must be included in the group.
--If the value immediately following a multiple of 5 is 1, it must not be chosen. (No loops needed.)

--groupSum5(0, [2, 5, 10, 4], 19) → true
--groupSum5(0, [2, 5, 10, 4], 17) → true
--groupSum5(0, [2, 5, 10, 4], 12) → false

import Data.Bool

groupSum5 :: [Int] -> Int -> Int -> Bool
groupSum5 (x:xs) target sum
      | x `mod` 5 == 0 && head xs == 1 = groupSum5 (tail xs) target (sum+x)
      | x `mod` 5 == 0                 = groupSum5 xs target (sum+x)
      | otherwise                      = groupSum5 xs target (sum+x) || groupSum5 xs target (sum)
groupSum5 [] target sum                = target == sum

