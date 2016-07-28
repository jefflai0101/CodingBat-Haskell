--Given a string, compute recursively (no loops) a new string where all the lowercase 'x' chars have been changed to 'y' chars.

--changeXY("codex")   → "codey"
--changeXY("xxhixx")  → "yyhiyy"
--changeXY("xhixhix") → "yhiyhiy"


changeXY :: [Char] -> [Char]
changeXY (x:xs)
      | x == 'x'  = 'y' : changeXY xs
      | otherwise = x : changeXY xs
changeXY []       = []

changeXY' :: [Char] -> [Char]
changeXY' = map (\x -> if x == 'x' then 'y' else x)
