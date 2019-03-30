-- input  addfrequency "aabdeeeedeffa"
--outpu [(2,'a'),(1,'b'),(1,'d'),(4,'e'),(1,'d'),(1,'e'),(2,'f'),(1,'a')]
combineDuble :: Eq a => [a]->[[a]]
combineDuble [] = []
combineDuble (x:xs) = do
             let (same,differ) = span (== x) xs
             (x:same):combineDuble differ
                 

addfrequency :: Eq a=> [a] ->[(Int,a)]
addfrequency xs = map (\x -> (length x,head x) ) (combineDuble xs)
