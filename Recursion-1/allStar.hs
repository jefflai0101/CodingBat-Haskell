--Given a string, compute recursively a new string where all the adjacent chars are now separated by a "*".

--allStar("hello") → "h*e*l*l*o"
--allStar("abc")   → "a*b*c"
--allStar("ab")    → "a*b"

allStar :: [Char] -> [Char]
allStar (x:[]) = x : []
allStar (x:xs) = x : '*' : allStar xs

allStar' :: [Char] -> [Char]
allStar' = init . foldl (\x y -> x ++ y : "*") []