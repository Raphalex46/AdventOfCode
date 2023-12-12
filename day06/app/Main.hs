module Main where

import Data.Char

-- Define a race type (RaceDuration, DistanceToBeat)
type Race = (Int, Int)

-- Parse the input string
parseInput :: [String] -> [Race]
parseInput strs =
  -- Simply zip both lists of the input
  case [filter (\x -> isDigit x || isSpace x) s | s <- strs] of
    [l1, l2] -> zip (map read $ words l1) (map read $ words l2)
    _ -> error "invalid input"

-- Transform problem 1 parsing into problem 2 parsing (basically, fuse
-- the different races of the input into one big race
transformRaces :: [Race] -> Race
transformRaces rs =
  let (ts, ds) = unzip rs
   in (fuse ts, fuse ds)
  where
    fuse = read . concat . map show

-- Counts the numbers of different ways to beat the record distance on a race
-- in the given time
waysToBeat :: Race -> Int
waysToBeat (t, d) =
  -- Probably not the best solution, but the trick I use is the following:
  -- Every possible distance achievable is: {i * (t - i) | i in {0..t}}
  -- This sequence is symmetric. We can start from the optimal value which
  -- is in the middle (i = t / 2) and test each value until we get a distance
  -- inferior to the record distance.
  -- The number of solutions obtained is then doubled and adjusted depending
  -- on whether t is odd or even (since this is symmetric)
  let mid = t `div` 2
      partOptimal = length $ takeWhile (> d) [a * b | (a, b) <- zip [(t - mid) .. t] $ reverse [0 .. mid]]
   in partOptimal * 2 - (t + 1) `mod` 2

solution1 :: [Race] -> Int
solution1 = product . map waysToBeat

solution2 :: [Race] -> Int
solution2 = waysToBeat . transformRaces

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      parsedInput = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
