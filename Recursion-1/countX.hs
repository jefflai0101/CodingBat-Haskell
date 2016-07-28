--Given a string, compute recursively (no loops) the number of lowercase 'x' chars in the string.

--countX("xxhixx")  → 4
--countX("xhixhix") → 3
--countX("hi")      → 0

countX :: [Char] -> Int
countX = foldl (\x y -> if y == 'x' then x+1 else x+0) 0
--countX charX = foldl (\x y -> if y == 'x' then x+1 else x+0) 0 charX

