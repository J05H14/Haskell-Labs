square :: Int -> Int
square x = x * x

cube :: Int -> Int
cube x = x * x * x

triple :: Int -> Int
triple x = x * 3

addNum :: (Num a) => a -> a -> a
addNum a b = a + b

stringAdd :: [Char] -> Float -> Float
stringAdd str flt = (read str) + flt

numOrder :: Int -> [Char]
numOrder x = if x == 1 then "Once"
    else if x == 2 then "Twice"
    else if x == 3 then "Thrice"
    else "Don't know how to say that in English."

first :: (a, b, c, d) -> a
first (x, _, _, _) = x

second :: (a, b, c, d) -> b
second (_, x, _, _) = x

third :: (a, b, c, d) -> c
third (_, _, x, _) = x

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

power :: Int -> Int -> Int
power x 1 = x
power a b = a * power a (b - 1)

addList :: [Int] -> Int
addList [] = 0
addList [a] = a
addList [a, b] = a + b
addList [a, b, c] = a + b + c
addList (a:b:c:_) = a + b + c