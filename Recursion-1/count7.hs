--Given a non-negative int n, return the count of the occurrences of 7 as a digit, so for example 717 yields 2. (no loops).
--Note that mod (%) by 10 yields the rightmost digit (126 % 10 is 6), while divide (/) by 10 removes the rightmost digit (126 / 10 is 12).

count7 :: Int -> Int
count7 0 = 0
count7 x
      | remainX == 7  = count7 (passOn) + 1
      | otherwise     = count7 (passOn)
        where remainX = x `mod` 10
              passOn  = x `div` 10

count7' :: Int -> Int
count7' = sum . map (\x -> if (x `mod` 10 == 7) then 1 else 0) . takeWhile (/=0) . iterate (\x -> x `div` 10)