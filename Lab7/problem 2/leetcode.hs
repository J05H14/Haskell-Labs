main = do
    putStrLn "Input a phrase:"
    phrase <- getLine
    let leetphrase = convertSymbols phrase
    putStrLn leetphrase

convertSymbols :: String -> String
convertSymbols [] = []
convertSymbols (x:xs)
    | x == 'o' = '0' : convertSymbols xs
    | x == 'e' = '3' : convertSymbols xs
    | x == 'a' = '@' : convertSymbols xs
    | x == 'l' = '1' : convertSymbols xs
    | otherwise = x : convertSymbols xs
