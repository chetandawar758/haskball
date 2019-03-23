{-- different random number --}
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  temp <- getStdGen
  return . take n $ randomRs(1, m)temp
