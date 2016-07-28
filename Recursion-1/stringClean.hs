--Given a string, return recursively a "cleaned" string where adjacent chars that are the same have been reduced to a single char.
--So "yyzzza" yields "yza".

--stringClean("yyzzza")  → "yza"
--stringClean("abbbcdd") → "abcd"
--stringClean("Hello")   → "Helo"

stringClean :: [Char] -> [Char]
stringClean (x:xs@(y:_))
      | x == y     = stringClean xs
      | otherwise  = x : stringClean xs
stringClean (x:[]) = x : []

stringClean' :: [Char] -> [Char]
stringClean' = foldl (\x y -> if (x == []) then x ++ [y] else if (y /= last x) then x ++ [y] else x) []