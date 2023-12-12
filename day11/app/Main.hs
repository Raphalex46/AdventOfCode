module Main where

import Data.List

-- A galaxy is just coordinates
type Galaxy = (Int, Int)

type Puzzle = ([Int], [Int], [Galaxy])

-- Everything is done at the parsing stage for this problem
parseInput :: [String] -> Puzzle
parseInput strs =
  -- Get the indices of empty rows / columns
  let expandedRows = getExpandedInd strs
      expandedCols = getExpandedInd $ transpose strs
      height = length strs - 1
      width = case strs of
        x : _ -> length x - 1
        _ -> error "Invalid input"
      galaxies = [(j, i) | j <- [0 .. width], i <- [0 .. height], ((strs !! i) !! j) == '#']
   in (expandedRows, expandedCols, galaxies)

-- Compute the expansion of the space (by modifying the galaxy coordinates)
computeExpansion :: Puzzle -> Int -> [Galaxy]
computeExpansion (expandedRows, expandedCols, galaxies) expFactor =
  map adjust galaxies
  where
    adjust (i, j) =
      ( j + (length $ takeWhile (< j) expandedCols) * (expFactor - 1),
        i + (length $ takeWhile (< i) expandedRows) * (expFactor - 1)
      )

-- Get the indices of expanded rows (empty rows)
getExpandedInd :: [String] -> [Int]
getExpandedInd strs =
  [i | (str, i) <- zip strs [0 ..], all (== '.') str]

-- Compute the distance between two galaxies
computeDistance :: Galaxy -> Galaxy -> Int
computeDistance (a, b) (c, d) =
  abs (a - c) + abs (b - d)

-- Sum the distance between galaxies
sumDistances :: [Galaxy] -> Int
sumDistances galaxies =
  -- I'm lazy so I just divie by 2 instead of avoiding duplicates
  (sum [computeDistance a b | a <- galaxies, b <- galaxies]) `div` 2

-- Solution to problem 1 uses an expand factor of 2
solution1 :: Puzzle -> Int
solution1 p =
  let expanded = computeExpansion p 2
   in sumDistances expanded

-- Solution to problem 2 uses an expand factor of 1,000,000
solution2 :: Puzzle -> Int
solution2 p =
  let expanded = computeExpansion p 1000000
   in sumDistances expanded

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      parsedInput = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
