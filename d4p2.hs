module Main where
import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    contents <- readFile "d4.txt"
    let grid = lines contents
        height = length grid
        width = if height > 0 then length (head grid) else 0
        initialTargets = Set.fromList [(r, c) | r <- [0..height-1], c <- [0..width-1], (grid !! r) !! c == '@']
        totalRemoved = removeAll initialTargets (height, width)
    print totalRemoved

removeAll :: Set (Int, Int) -> (Int, Int) -> Int
removeAll targets dimensions
    | Set.null accessible = 0
    | otherwise = Set.size accessible + removeAll remaining dimensions
    where
        accessible = Set.filter (isAccessible targets dimensions) targets
        remaining = Set.difference targets accessible

isAccessible :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isAccessible targets (height, width) (r, c) =
    let neighbours = [(r+dr, c+dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]
        validNeighbours = filter (\(nr, nc) -> nr >= 0 && nr < height && nc >= 0 && nc < width) neighbours
        adjacentTargets = length [() | pos <- validNeighbours, Set.member pos targets]
    in adjacentTargets < 4