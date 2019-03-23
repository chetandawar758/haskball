{-- is given element present in list or not --}
isElement ::Eq a => a -> [a] -> Bool
isElement x [] = False
isElement x xs 
       | x == (head xs) = True
       | otherwise = isElement x (tail xs)
