generation :: Int -> String
generation year
    | year > 1995 = "Generation Z"
    | year > 1980 = "Millenial"
    | year > 1965 = "Generation X"
    | year > 1945 = "Baby Boomer"
    | year > 1933 = "Silent Generation"
    | otherwise = "You don't get one"

generation2 :: Int -> String
generation2 age
    | year > 1995 = "Generation Z"
    | year > 1980 = "Millenial"
    | year > 1965 = "Generation X"
    | year > 1945 = "Baby Boomer"
    | year > 1933 = "Silent Generation"
    | otherwise = "You don't get one"
    where year = 2017 - age

hypo :: [(Float, Float)] -> [Float]
hypo xs = [hypVal a b | (a, b) <- xs]
    where 
    hypVal a b = sqrt (a*a + b*b)

piMulti :: Float -> [Float] -> [Float]
piMulti a xs = 
    let piA = a * pi
    in [z * piA| z<- xs]

addThree xs = case xs of [] -> 0
                         [x] -> x
                         [x, y] -> x + y
                         [x, y, z] -> x + y + z
                         (x:y:z:_) -> x + y + z