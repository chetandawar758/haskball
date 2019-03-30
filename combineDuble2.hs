
--Input is like : combineDuble  "aabbcdaaddeffg"
-- combineDuble is function with input string.
-- Output is : ["aa","bb","c","d","aa","dd","e","ff","g"]
removeduble :: [Char]->[Char]
removeduble [] = []
removeduble (x:xs) = x:removeduble(dropWhile (==x) xs)


run :: [Char]->[Char]->[Char]
run [] ys = []
run  xs ys = if((head xs)==(head ys))
             then 
                (head xs):run (tail xs) ys
             else
                ' ':(head xs):run (tail xs) (tail ys) 



combineDuble :: [Char]->[[Char]]
combineDuble xs = words(run xs (combineDuble xs))
