--Given a string, compute recursively a new string where identical chars that are adjacent in the original string are separated from each other by a "*".

--pairStar("hello") → "hel*lo"
--pairStar("xxyy")  → "x*xy*y"
--pairStar("aaaa")  → "a*a*a*a"

pairStar :: [Char] -> [Char]
pairStar (x:[])   = x : []
pairStar (x:y:xs)
      | x == y    = x : '*' : pairStar (y:xs)
      | otherwise = x : pairStar (y:xs)

pairStar' :: [Char] -> [Char]
pairStar' wStr = init . foldl (\x y -> if (head y == last y) then x ++ [head y] ++ "*" else x ++ [head y]) [] . takeWhile (\x -> length x > 0) $ map (\x -> take 2 $ drop x wStr) [0..length wStr]