module Main where
import Data.Char (isSpace, isDigit, digitToInt)
import Data.List (dropWhileEnd)

main :: IO ()
main = do
    contents <- readFile "d6.txt"
    let rows = lines contents
        opRow = last rows
        numRows = init rows
        width = maximum $ map length rows
        problems = cols rows width
        solve (sIdx, eIdx) =
            let getCol i = [if i < length r then r !! i else ' ' | r <- numRows]
                read col = let digits = [digitToInt c | c <- col, isDigit c]
                                   in foldl (\acc d -> acc * 10 + toInteger d) 0 digits
                vals = [read (getCol i) | i <- [eIdx, eIdx-1..sIdx]]
                op = head $ filter (`elem` "+*") (slice opRow sIdx eIdx)
                slice s sIdx eIdx = take (eIdx - sIdx + 1) (drop sIdx s)
            in (if op == '+' then sum else product) vals
    print $ sum $ map solve problems

cols :: [String] -> Int -> [(Int, Int)]
cols rows width = let hasContent i = any (\r -> i < length r && r !! i /= ' ') rows
                      get i | i >= width = []
                            | not (hasContent i) = get (i + 1)
                            | otherwise = let end = head [j | j <- [i..width-1], not (hasContent (j+1))]
                                            in (i, end) : get (end + 1)
                  in get 0
