{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Array
import Data.Char
import Data.List

-- Data structure to represent a cell in the input matrix
-- The 'Num' constructor is special as it hold both the number' value (Int) and
-- a special key ((Int, Int)) which is the coordinates of the first digit of
-- the number. This allows for identifying unique numbers since number digits
-- take multiple characters in the input matrix
data Cell = Gear | Num (Int, Int) Int | Sym | Empty deriving (Eq, Show)

-- Various functions to test and filter cells
isNum :: Cell -> Bool
isNum (Num _ _) = True
isNum _ = False

isSym :: Cell -> Bool
isSym Gear = True
isSym Sym = True
isSym _ = False

isGear :: Cell -> Bool
isGear Gear = True
isGear _ = False

-- Type of the input matrix
type PuzzleMatrix = Array (Int, Int) Cell

-- Parse the input string to an input matrix
parseInput :: [String] -> PuzzleMatrix
parseInput s =
  let parsed = concat (zipWith (parseLine' 0) [0 .. (length s) - 1] s)
   in case parsed of
        l@(x : _) -> array (fst x, fst (last l)) parsed
        [] -> array ((0, 0), (0, 0)) []
  where
    parseLine' j i s = parseLine i j s

-- Parse each line in the input, filling the cell value and the index of each
-- cell
parseLine :: Int -> Int -> String -> [((Int, Int), Cell)]
parseLine i j s@(x : xs)
  -- We simply create the cell for each different character
  | isDigit x =
      -- Numbers are a bit special since they can occupy multiple characters:
      -- when encountering a digit, we parse the number and fill both the index
      -- of each digit, and create the number cell. The number cell has a
      -- unique key, which is the coordinate of the first digit of the number.
      -- That way, numbers can be filtered to avoid duplicate
      let numString = (takeWhile isDigit s)
          num = read numString
          len = length numString
       in [((i, j_l), Num (i, j) num) | j_l <- [j .. (j + len - 1)]]
            ++ parseLine i (j + len) (dropWhile isDigit s)
  | otherwise = parseSymbol x : parseLine i (j + 1) xs
  where
    -- Parsing the other symbols is straightforward
    parseSymbol '.' = ((i, j), Empty)
    parseSymbol '*' = ((i, j), Gear)
    parseSymbol _ = ((i, j), Sym)
parseLine _ _ [] = []

-- Returns the neighbors of a given cell in the matrix
neighbors :: PuzzleMatrix -> (Int, Int) -> [Cell]
neighbors m (i, j) =
  let ((lbx, lby), (ubx, uby)) = bounds m
   in [ (m ! (i + x, j + y))
        | x <- [-1 .. 1],
          y <- [-1 .. 1],
          let nx = i + x; ny = j + y,
          x /= 0 || y /= 0,
          nx >= lbx,
          nx <= ubx,
          ny >= lby,
          ny <= uby
      ]

-- Returns the valid numbers in the matrix: numbers which are adjacent to a
-- symbol
getValidNumbers :: PuzzleMatrix -> [Cell]
getValidNumbers m =
  let validNums = filter (\((i, j), _) -> not (null (filter isSym (neighbors m (i, j))))) (filter (isNum . snd) (assocs m))
   in nub (map snd validNums)

-- Returns the product of numbers that are adjacent to a gear ('*' character)
-- only if there are exactly 2 numbers adjacent to the gear
getGearRatios :: PuzzleMatrix -> [Int]
getGearRatios m =
  -- We filter to get the gears, then for each gear, get their part number and
  -- multiply them
  map (\(a, b) -> a * b) (map (getPartNumbers m) [i | (i, e) <- (assocs m), isGear e])
  where
    -- This function returns the part numbers of a given gear, or returns (0,
    -- 0) if there is not exactly 2 adjacent numbers to the gear (since we sum
    -- everything in the end, this changes nothing)
    getPartNumbers m (i, j) =
      let neighborNumbers = [n | Num _ n <- nub (filter isNum (neighbors m (i, j)))]
       in case neighborNumbers of
            [a, b] -> (a, b)
            _ -> (0, 0)

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      matrix = parseInput input
  putStrLn ("Solution to problem 1: " ++ (show (sum [x | Num _ x <- (getValidNumbers matrix)])))
  putStrLn ("Solution to problem 2: " ++ (show (sum (getGearRatios matrix))))
