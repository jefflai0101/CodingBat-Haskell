--Given a string and a non-empty substring sub, compute recursively the largest substring which starts and ends with sub and return its length.

--strDist("catcowcat", "cat")     → 9
--strDist("catcowcat", "cow")     → 3
--strDist("cccatcowcatxx", "cat") → 9

strDist :: [Char] -> [Char] -> Int -> Int
strDist wStr wSub 1
     | length wStr == 0                                = 0
     | take (length wSub) wStr == wSub                 = strDist wStr wSub 2
     | otherwise                                       = strDist (drop 1 wStr) wSub 1
strDist wStr wSub 2
     | length wStr == 0                                = 0
     | drop (length wStr - (length wSub)) wStr == wSub = length wStr
     | otherwise                                       = strDist (take (length wStr -1) wStr) wSub 2