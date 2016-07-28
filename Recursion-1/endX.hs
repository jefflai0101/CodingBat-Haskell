--Given a string, compute recursively a new string where all the lowercase 'x' chars have been moved to the end of the string.

--endX("xxre")    → "rexx"
--endX("xxhixx")  → "hixxxx"
--endX("xhixhix") → "hihixxx"

endX :: [Char] -> [Char]
endX (x:xs)
     | x == 'x'  = endX xs ++ [x]
     | otherwise = [x] ++ endX xs
endX x           = x
