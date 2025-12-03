module Main where
import Data.Char (digitToInt)
import Data.List (tails)

main :: IO ()
main = do
    contents <- readFile "d3.txt"
    print $ sum $ map maxValue $ lines contents

maxValue :: String -> Int
maxValue line = maximum [digitToInt a * 10 + digitToInt b | (a:rest) <- tails line, b <- rest]