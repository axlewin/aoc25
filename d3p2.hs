module Main where
import Data.Char (digitToInt)

main :: IO ()
main = do
    contents <- readFile "d3.txt"
    print $ sum $ map maxValue $ lines contents

selectMax :: Int -> String -> String
selectMax 0 _ = ""
selectMax k str
    | k >= length str = str
    | otherwise = 
        let needed = k - 1
            skip = length str - k 
            options = take (skip + 1) str
            maxDigit = maximum options
            pos = length $ takeWhile (/= maxDigit) options
        in maxDigit : selectMax needed (drop (pos + 1) str)

maxValue :: String -> Int
maxValue line = read $ selectMax 12 line
