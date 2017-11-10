import Data.List
import Data.Ord

--1
isDiv :: Int -> Int -> Bool
isDiv n d = (d `mod` n) == 0
isDivBy5 :: Int -> Bool
isDivBy5 = isDiv 5

--2
betweenAF :: Int -> [Char] -> Bool
betweenAF n "" =  True
betweenAF n str 
    | head str `elem` ['a' .. 'f'] = betweenAF n (drop n str)
    | head str `elem` ['A' .. 'F'] = betweenAF n (drop n str)
    | otherwise = False
otherBetweenAF :: String -> Bool
otherBetweenAF = betweenAF 2

--3
zipAndAdd :: Num b => [b] -> [b] -> [b]
zipAndAdd [] _ = []
zipAndAdd _ [] = []
zipAndAdd (x:xs) (y:ys) = (fst $ head zipped) + (snd $ head zipped) : zipAndAdd xs ys
    where zipped = zip (x:xs) (y:ys) 

--4
removeLaterWords :: [String] -> [String]
removeLaterWords [] = []
removeLaterWords (x:xs) = takeWhile (/= ' ') x : removeLaterWords xs

--5
emptyToZero :: [[String]] -> [[String]]
emptyToZero [] = []
emptyToZero ((x:xs) : xss) = emptyOneList (x:xs) : emptyToZero xss

emptyOneList :: [String] -> [String]
emptyOneList [] = []
emptyOneList (x:xs)
    | x == "" = "0" : emptyOneList xs
    | otherwise = x : emptyOneList xs

--6
tuplePairs :: [a] -> [(a,a)]
tuplePairs [] = []
tuplePairs [x] = []
tuplePairs (a:b:xs) = (a,b) : tuplePairs xs

--7
addPairs :: Num a => [a] -> [a]
addPairs (x:xs) = foldl (\a (x, y) -> a ++ [x*y] ) [] tList
    where tList = tuplePairs (x:xs)

--8
cumSum :: [Int] -> [Int]
cumSum (x:xs) = scanl (+) 0 (x:xs)

--9
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f $ f $ f x

--10
isLowerCase :: Char -> Bool
isLowerCase = (`elem` ['a' .. 'z'])

--11
sortByLength :: [String] -> [String]
sortByLength = sortBy sortFirst 
    where
        sortFirst = comparing (length . head . words)

--12
pack :: [Char] -> [String]
pack (x:xs) = group $ sort (x:xs)