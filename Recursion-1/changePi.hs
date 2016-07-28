--Given a string, compute recursively (no loops) a new string where all appearances of "pi" have been replaced by "3.14".

--changePi("xpix") → "x3.14x"
--changePi("pipi") → "3.143.14"
--changePi("pip")  → "3.14p"

changePi :: [Char] -> [Char]
changePi (x:y:xs)
      | x == 'p' && y == 'i'  = "3.14" ++ changePi (xs)
      | otherwise             = x : changePi (y:xs)
changePi (x:[])               = x : changePi ([])
changePi []                   = []


--Incorrect
changePi' :: [Char] -> [Char]
changePi' wStr = filter (/= '\n') . unlines $ map (\x -> if ((take 2 $ drop x wStr) == "pi") then "3.14" else (take 1 $ drop x wStr)) [0..length wStr]