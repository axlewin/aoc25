module Main where
import System.IO
import Data.Array ( Array, (!), listArray )

main :: IO ()
main = do
    contents <- readFile "d4.txt"
    let grid = lines contents
        height = length grid
        width = if height > 0 then length (head grid) else 0
        arr = listArray ((0, 0), (height - 1, width - 1)) (concat grid)
        target = [(r, c) | r <- [0..height-1], c <- [0..width-1], arr ! (r, c) == '@']
        accessible = filter (isAccessible arr (height, width)) target
    print $ length accessible

isAccessible :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
isAccessible arr (height, width) (r, c) =
    let neighbors = [(r+dr, c+dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]
        validNeighbours = filter (\(nr, nc) -> nr >= 0 && nr < height && nc >= 0 && nc < width) neighbors
        adjacentTargets = length [() | (nr, nc) <- validNeighbours, arr ! (nr, nc) == '@']
    in adjacentTargets < 4