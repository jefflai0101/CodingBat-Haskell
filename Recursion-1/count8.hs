--Given a non-negative int n, compute recursively (no loops) the count of the occurrences of 8 as a digit, 
--except that an 8 with another 8 immediately to its left counts double, so 8818 yields 4.
--Note that mod (%) by 10 yields the rightmost digit (126 % 10 is 6), while divide (/) by 10 removes the rightmost digit (126 / 10 is 12).

--count8(8)    → 1
--count8(818)  → 2
--count8(8818) → 4

count8 :: Int -> Int
count8 0 = 0
count8 x
      | remainXX == 88          = count8 passOn + 2
      | remainX == 8            = count8 passOn + 1
      | otherwise               = count8 passOn
        where remainX  = x `mod` 10
              remainXX = x `mod` 100
              passOn   = x `div` 10

count8' :: Int -> Int
count8' = sum . map (\x -> if (x `mod` 100 == 88) then 2 else if (x `mod` 10 == 8) then 1 else 0). takeWhile (/=0) . iterate (\x -> x `div` 10)
