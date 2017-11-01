summationInverse :: Int -> Float
summationInverse 1 = 1
summationInverse i = (1 / (fromIntegral i)) + summationInverse (i - 1)

summationInverseFrac :: Int -> Float
summationInverseFrac 1 = 0.5
summationInverseFrac i = (fromIntegral i / (fromIntegral (i + 1))) + summationInverseFrac (i - 1)

removeMax :: [Int] -> [Int]
removeMax [] = []
removeMax (x:xs) 
    |x == maximum(x:xs) = xs
    |otherwise = x : removeMax xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs)
    |(x:xs) == [x] = [x]
    |x == maximum (x:xs) = sort xs ++ [x]
    |otherwise = sort (xs ++ [x])
