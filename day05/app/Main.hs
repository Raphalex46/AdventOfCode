{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Char

-- Type alias for ranges of numbers
type Range = (Int, Int)

-- The representation of a map line in the input file: it has a source start,
-- a destination start and a length
data RangeMap = RangeMap Int Int Int

type Puzzle = ([Range], [[RangeMap]])

-- Applies a range map to a range. It returns transformed part of the input
-- range that fits withing the map bounds, or returns Nothing if nothing maps
-- correctly
applyMap :: RangeMap -> Range -> Maybe Range
applyMap em (st, en)
  | nss <= nse = Just (nds, nde)
  | otherwise = Nothing
  where
    (ss, se) = sourceRange em
    (ds, _) = destRange em
    nss = max st ss
    nse = min en se
    nds = ds + (nss - ss)
    nde = nds + (nse - nss)

-- Access function for maps (start and end of the source range)
sourceRange :: RangeMap -> Range
sourceRange (RangeMap _ ss rl) = (ss, ss + rl - 1)

-- Access function for maps (start and end of the dest range)
destRange :: RangeMap -> Range
destRange (RangeMap ds _ rl) = (ds, ds + rl - 1)

-- Parse the seed numbers in the input file
parseSeeds :: String -> [Range]
parseSeeds s =
  map (\x -> let n = read x in (n, n)) $ words $ filter (\x -> isDigit x || isSpace x) s

-- Parse the maps in the input file
parseMap :: String -> RangeMap
parseMap s =
  case (map read $ words $ s) of
    [ds, ss, rl] -> RangeMap ds ss rl
    _ -> error "incorrect map in input"

-- Parse input
parseInput :: [String] -> Puzzle
parseInput (x : xs) =
  let seeds = parseSeeds x
      maps = parseAllMaps $ filter (/= "") xs
   in (seeds, maps)
  where
    parseAllMaps (_ : xs) =
      let (maps, rest) = break (any isAlpha) xs
       in (map parseMap maps) : parseAllMaps rest
    parseAllMaps [] = []
parseInput [] = ([], [])

-- Function to transformed the parsed seeds from problem 1 to the seed ranges
-- of problem 2
transformSeeds :: [Range] -> [Range]
transformSeeds r =
  let len = length r - 1
   in [ (a, a + (b - 1))
        | (a, b) <-
            zip (map (fst . (r !!)) [0, 2 .. len]) (map (fst . (r !!)) [1, 3 .. len])
      ]

-- Run a range through a list of maps, resulting in multiple ranges
runThroughMaps :: Range -> [RangeMap] -> [Range]
runThroughMaps r@(os, oe) (m : ms)
  | os > oe = []
  | otherwise =
      case (applyMap m r) of
        Just ne ->
          let (ss, se) = sourceRange m
           in ne : ((runThroughMaps (os, (ss - 1)) ms) ++ runThroughMaps ((se + 1), oe) ms)
        Nothing -> runThroughMaps r ms
runThroughMaps r@(os, oe) []
  | os > oe = []
  | otherwise = [r]

-- Run every range in the puzzle through every map
runAllThroughMaps :: Puzzle -> [Range]
runAllThroughMaps (r, (m : ms)) =
  let newRanges = concat $ map (flip runThroughMaps m) r
   in runAllThroughMaps (newRanges, ms)
runAllThroughMaps (r, []) = r

-- Solution to part 1
solution1 :: Puzzle -> Int
solution1 = minimum . (map fst) . runAllThroughMaps

-- Solution to part 2
solution2 :: Puzzle -> Int
solution2 (r, m) = minimum . (map fst) . runAllThroughMaps $ (transformSeeds r, m)

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      parsedInput = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
