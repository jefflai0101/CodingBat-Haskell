--Count recursively the total number of "abc" and "aba" substrings that appear in the given string.

--countAbc("abc")      → 1
--countAbc("abcxxabc") → 2
--countAbc("abaxxaba") → 2

countAbc :: [Char] -> Int
countAbc (x:xs@(xsh:xst:_))
      | aT && bT && cT = 1 + countAbc xs
      | otherwise      = countAbc xs
      where aT = x == 'a'
            bT = xsh == 'b'
            cT = (xst == 'c' || xst == 'a')
countAbc (x:y:[])                         = 0


--countAbc :: [Char] -> Int
--countAbc (x:xs@(xsh:xst:_))
--      | x == 'a' && xsh == 'b' && xst == 'c' = 1 + countAbc xs
--      | x == 'a' && xsh == 'b' && xst == 'a' = 1 + countAbc xs
--      | otherwise                            = countAbc xs
--countAbc (x:y:[])                            = 0