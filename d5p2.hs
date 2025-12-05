module Main where
import System.IO
import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "d5.txt"
    let rangesStr = takeWhile (not . null) (lines contents)
        ranges = map parseRange rangesStr
        mergedRanges = mergeRanges (sort ranges)
        totalGood = sum [b - a + 1 | (a, b) <- mergedRanges]
    print totalGood

parseRange :: String -> (Integer, Integer)
parseRange s =
    let (a, b) = break (== '-') s
    in (read a, read (tail b))

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges ((a, b) : (c, d) : rest)
    | b + 1 >= c = mergeRanges ((a, max b d) : rest)
    | otherwise = (a, b) : mergeRanges ((c, d) : rest)
