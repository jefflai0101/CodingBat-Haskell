--Given a string, compute recursively a new string where all the 'x' chars have been removed.

--noX("xaxb") → "ab"
--noX("abc")  → "abc"
--noX("xx")   → ""

noX :: [Char] -> [Char]
noX = filter (/= 'x')