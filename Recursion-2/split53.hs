--Given an array of ints, is it possible to divide the ints into two groups, so that the sum of the two groups is the same, with these constraints:
--all the values that are multiple of 5 must be in one group, and all the values that are a multiple of 3 (and not a multiple of 5) must be in the other. (No loops needed.)

--split53([1, 1])    → true
--split53([1, 1, 1]) → false
--split53([2, 4, 2]) → true

import Data.Bool

split53 :: [Int] -> Bool
split53 nArray     = helper allThree allFive otherN
    where allFive  = sum $ filter (\x -> x `mod` 5 == 0) nArray                        --Grouping all 5 multipliers
          allThree = sum $ filter (\x -> x `mod` 3 == 0 && x `mod` 5 /= 0) nArray      --Grouping all 3 but not 5 multiplers
          otherN   = filter (\x -> x `mod` 3 /= 0 && x `mod` 5 /= 0) nArray            --Grouping everything else

helper :: Int -> Int -> [Int] -> Bool
helper g3 g5 []     = g3 == g5
helper g3 g5 others = helper (g3 + head others) g5 (tail others) || helper g3 (g5 + head others) (tail others)
