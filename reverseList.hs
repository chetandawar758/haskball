{-- reverse the list --}
rev :: [a] -> [a] 
rev [] = []
rev xs = (last xs) : rev (init xs)
