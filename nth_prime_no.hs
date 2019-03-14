nth_prime n = all_primes !! (n - 1)
  where
    all_primes = filterPrime [2..]
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
    
    {- n is input . -}
