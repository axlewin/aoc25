module Main where
import System.IO (readFile)
import Data.List (mapAccumL)

main :: IO ()
main = do
	contents <- readFile "d1.txt"
	let
		instructions = lines contents
		deltas = map directedDelta instructions
		crossings = snd $ mapAccumL signChanges 50 deltas
	print $ sum crossings

directedDelta :: String -> Int
directedDelta (direction:num) =
	let magnitude = read num :: Int
	in case direction of
		'R' -> magnitude
		'L' -> -magnitude

signChanges :: Int -> Int -> (Int, Int)
signChanges position delta =
  let end = position + delta
      zeroCrossings
        | delta > 0 = length [k | k <- [position + 1 .. end], mod k 100 == 0]
        | delta < 0 = length [k | k <- [end .. position - 1], mod k 100 == 0]
        | otherwise = 0
      newPosition = mod end 100
  in (newPosition, zeroCrossings)
  