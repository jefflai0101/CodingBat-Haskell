--Given a string and a non-empty substring sub, compute recursively the number of times that sub appears in the string,
--without the sub strings overlapping.

--strCount("catcowcat", "cat") → 2
--strCount("catcowcat", "cow") → 1
--strCount("catcowcat", "dog") → 0

strCount :: [Char] -> [Char] -> Int
strCount strSet subStr
     | subStr == take subLen strSet = 1 + strCount (drop subLen strSet) subStr
     | strSet == []                 = 0
     | otherwise                    = strCount (drop subLen strSet) subStr
     where subLen = length subStr

strCount' :: [Char] -> [Char] -> Int
strCount' wStr tSub = sum $ map (\x -> if (take 3 $ drop (x * 3) wStr) == tSub then 1 else 0) [0..length wStr `div` 3]