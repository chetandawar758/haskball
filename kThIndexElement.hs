{-- Kth index element of list --}
kthelem :: Int -> [a] -> Int
kthelem 0 xs = head xs
kthelem k [] = error "No such index Exist"
kthelem  k xs = kthelem (k-1) (tail xs)
