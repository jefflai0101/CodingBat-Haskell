--Given a non-negative int n, return the sum of its digits recursively (no loops).
--Note that mod (%) by 10 yields the rightmost digit (126 % 10 is 6), while divide (/) by 10 removes the rightmost digit (126 / 10 is 12).

--sumDigits(126) → 9
--sumDigits(49)  → 13
--sumDigits(12)  → 3

sumDigits :: Int -> Int
sumDigits n
       | n == 0    = 0
       | otherwise = sumDigits (n `div` 10) + (n `mod` 10)
