isDiv :: Int -> Int -> Bool
isDiv n d = (d `mod` n) == 0
isDivBy5 :: Int -> Bool
isDivBy5 = isDiv 5

betweenAF :: Int -> [Char] -> Bool
betweenAF n "" =  True
betweenAF n str 
    | head str `elem` ['a' .. 'f'] = betweenAF n (drop n str)
    | head str `elem` ['A' .. 'F'] = betweenAF n (drop n str)
    | otherwise = False

otherBetweenAF :: String -> Bool
otherBetweenAF = betweenAF 2

zipAndAdd :: Num b => [b] -> [b] -> [b]
zipAndAdd (x:xs) (y:ys) = map (uncurry (+)) zipped
    where zipped = zip (x:xs) (y:ys)

removeLaterWords :: [String] -> [String]
removeLaterWords [] = []
removeLaterWords (x:xs) = takeWhile (/= ' ') x : removeLaterWords xs

emptyToZero ((x:xs):(x:xs)s) = 1