module Main where
import System.IO
import Data.List
import Data.Char

main :: IO ()
main = do
    contents <- readFile "d2.txt"
    let 
        ranges = readInput contents
        bad = concatMap (filter isBad . expandRange) ranges
    print (sum bad)

readRange :: String -> (Int, Int)
readRange s =
    let (a, b) = break (== '-') s
    in (read a, read (tail b))

readInput :: String -> [(Int, Int)]
readInput = map readRange . split ',' . filter (/= '\n')

expandRange :: (Int, Int) -> [Int]
expandRange (a, b) = [a .. b]

isBad :: Int -> Bool
isBad n =
    let s = show n
        len = length s
    in any (\patternLen ->
        mod len patternLen == 0 &&
        div len patternLen >= 2 &&
        let pattern = take patternLen s
            repeated = concat (replicate (div len patternLen) pattern)
        in s == repeated
        ) [1 .. div len 2]

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split d xs =
    let (h, t) = break (== d) xs
    in h : case t of
              [] -> []
              (_:rest) -> split d rest
