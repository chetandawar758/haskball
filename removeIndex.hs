--remove given index element
removeIndex. :: [a] -> Int -> [a]
removeIndex. [] _ = []
removeIndex. xs 0 = remduble (tail xs) (-1)
removeIndex. xs n = (head xs) : remduble (tail xs) (n-1)
