--Given an array of ints, is it possible to choose a group of some of the ints, beginning at the start index,
--such that the group sums to the given target?
--However, with the additional constraint that all 6's must be chosen. (No loops needed.)

--groupSum6(0, [5, 6, 2], 8) → true
--groupSum6(0, [5, 6, 2], 9) → false
--groupSum6(0, [5, 6, 2], 7) → false

import Data.Bool

groupSum6 :: [Int] -> Int -> Int -> Bool
groupSum6 (x:xs) target sum
      | x == 6              = groupSum6 xs target (sum+x)
      | otherwise           = groupSum6 xs target (sum+x) || groupSum6 xs target (sum)
groupSum6 [] target sum     = target == sum
