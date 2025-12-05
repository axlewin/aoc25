module Main where
import System.IO

main :: IO ()
main = do
    contents <- readFile "d5.txt"
    let (rangesStr, idsStr) = break null (lines contents)
        ranges = map parseRange rangesStr
        ids = map read $ filter (not . null) (tail idsStr)
        goodCount = length $ filter (isGood ranges) ids
    print goodCount

parseRange :: String -> (Integer, Integer)
parseRange s =
    let (a, b) = break (== '-') s
    in (read a, read (tail b))

isGood :: [(Integer, Integer)] -> Integer -> Bool
isGood ranges id = any (\(a, b) -> id >= a && id <= b) ranges