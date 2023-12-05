{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Data.Char
import Data.Array
import Data.List

data Cell = Gear | Num (Int, Int) Int | Sym | Empty deriving (Eq, Show)
isNum :: Cell -> Bool
isNum (Num _ _) = True
isNum _ = False

isSym :: Cell -> Bool
isSym Gear = True
isSym Sym = True
isSym _ = False

type PuzzleMatrix = Array (Int, Int) Cell

parseInput :: [String] -> PuzzleMatrix
parseInput s =
  let parsed = concat (zipWith (parseLine' 0) [0..(length s) -1] s)
   in array (fst (head parsed), fst (last parsed)) parsed
    where parseLine' j i s = parseLine i j s

parseLine :: Int -> Int -> String -> [((Int, Int), Cell)]
parseLine i j s@(x:xs)
  | isDigit x = let numString = (takeWhile isDigit s)
                    num = read numString
                    len = length numString
                in [((i, j_l), Num (i, j) num) | j_l <- [j..(j + len - 1)]]
                ++ parseLine i (j + len) (dropWhile isDigit s)
  | otherwise = parseSymbol x:parseLine i (j + 1) xs
  where parseSymbol '.' = ((i, j), Empty)
        parseSymbol '*' = ((i, j), Gear)
        parseSymbol _ = ((i, j), Sym)
parseLine _ _ [] = []

neighbors :: PuzzleMatrix -> (Int, Int) -> [Cell]
neighbors m (i, j) =
  let ((lbx, lby), (ubx, uby)) = bounds m
   in 
  [(m ! (i + x, j + y)) |
    x <- [-1..1], y <- [-1..1],
    let nx = i + x; ny = j + y, x /= 0 || y /= 0,
    nx >= lbx, nx <= ubx, ny >= lby, ny <=ubx]

getValidNumbers :: PuzzleMatrix -> [Cell]
getValidNumbers m =
  let l = filter (\((i, j), Num _ _) -> not (null (filter isSym (neighbors m (i, j)))) ) (filter (isNum . snd) (assocs m))
   in nub (map snd l)

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      matrix = parseInput input
  putStrLn ("Solution to problem 1: " ++ (show (sum (map (\(Num _ i) -> i) (getValidNumbers matrix)))))
