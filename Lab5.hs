findNext :: Int -> [Int] -> Int
findNext a [] = -1
findNext a (x:xs) 
    |x == a = nextVal
    |otherwise = findNext a xs
    where nextVal = if xs == [] then -1
                    else head xs

findPrev :: Int -> [Int] -> Int
findPrev a [] = -1
findPrev a (x:xs) 
    |prevVal == a = x
    |otherwise = findPrev a xs
    where prevVal = if xs == [] then -1
                    else head xs

digitSum :: Int -> Int
digitSum 0 = 0
digitSum a =
    let firstDigit = a `mod` 10
        rest = digitSum (a `div` 10)
    in firstDigit + rest