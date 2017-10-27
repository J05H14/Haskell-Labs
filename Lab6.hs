addInt :: Int -> Int -> Int
addInt a b = a + b

addTo10 :: Int -> Int
addTo10 = addInt 10

isLowerCase :: Char -> Bool
isLowerCase = (`elem` ['a' .. 'z'])

changeList :: (Int -> Int) -> [Int] -> [Int]
changeList f [] = []
changeList f [x] = [x]
changeList f (x:s:xs) = x:(f s):(changeList f xs)

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' _ [] _ _ = []
zipWith3' _ _ [] _ = []
zipWith3' _ _ _ [] = []
zipWith3' f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3' f xs ys zs

toTuple :: (Int -> Int) -> Int -> (Int, Int) 
toTuple f x = (x, f x)

map' :: (Int -> Int) -> [Int] -> [(Int, Int)]
map' _ [] = []
map' f [x] = [(x, f x)]
map' f (x:xs) = (x, f x) : map' f xs