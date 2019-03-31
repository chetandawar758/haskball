-- Input List : ["a", "b", "c", "d", "e", "f", "g", "h"] 3
--Output List : ["d","e","f","g","h","a","b","c"]
--Input List  : ["d","e","f","g","h","a","b","c"] (-2)
--Output List : ["d","e","f","g","h","a","b","c"]
rotateList :: [a] ->Int -> [a]
rotateList xs n 
           | n<0         = reverse(rotateList (reverse xs) (-n))                       
           |otherwise    = do
                           let (leftside ,rightside ) = splitAt (n `mod` (length xs)) xs
                           rightside ++ leftside
