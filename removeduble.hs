--Input is list : "aaabcdefaabghik"
--Output is     : "abcdefabghik"
removeduble ::Eq a=> [a]->[a]
removeduble [] = []
removeduble (x:xs) = x:removeduble(dropWhile (==x) xs)
