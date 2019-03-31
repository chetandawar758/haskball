--Extract a slice from a list
findInter :: [a] ->Int->Int -> [a]
findInter xs start end 
           | start < 0 = error "Your starting index is Wrong " 
           | end > length xs = error "Your Last index is outoff Bound"
           | otherwise =  do
                          let (left , right) = splitAt (start-1) xs
                              (left2 ,right2) = splitAt (end-start) right
                          left2 
