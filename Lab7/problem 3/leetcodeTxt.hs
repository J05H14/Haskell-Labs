import System.IO
import Data.Char

main = do
    contents <- readFile "input.txt"
    let newText = convertSymbols contents
    writeFile "leet.txt" (newText)

convertSymbols :: String -> String
convertSymbols [] = []
convertSymbols (x:xs)
    | x == 'o' = '0' : convertSymbols xs
    | x == 'e' = '3' : convertSymbols xs
    | x == 'a' = '@' : convertSymbols xs
    | x == 'l' = '1' : convertSymbols xs
    | otherwise = x : convertSymbols xs
