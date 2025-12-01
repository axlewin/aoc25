module Main where
import System.IO (readFile)

main :: IO ()
main = do
	contents <- readFile "d1.txt"
	let
		instructions = lines contents
		deltas = map directedDelta instructions
		positions = scanl step 50 deltas
		countZeros = length $ filter (== 0) positions
	print countZeros

directedDelta :: String -> Int
directedDelta (direction:num) =
	let magnitude = read num :: Int
	in case direction of
		'R' -> magnitude
		'L' -> -magnitude

step :: Int -> Int -> Int
step position delta = mod (position + delta) 100
