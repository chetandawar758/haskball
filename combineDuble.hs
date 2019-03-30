--Input is like :"aabbcdaaddeffg"
-- Output is : ["aa","bb","c","d","aa","dd","e","ff","g"]
combineDuble :: Eq a => [a]->[[a]]
combineDuble [] = []
combineDuble (x:xs) = do
                  let (same,differ) = span (== x) xs
                  (x:same):combineDuble differ
