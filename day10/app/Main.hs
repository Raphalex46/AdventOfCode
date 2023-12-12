{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Array

-- Directions (East, South, West, North)
data Direction = E | S | W | N deriving (Show, Eq)

-- Some helper functions for directions

-- Global list of directions
directions :: [Direction]
directions = [E, S, W, N]

-- Get the opposite of a direction
opposite :: Direction -> Direction
opposite S = N
opposite N = S
opposite E = W
opposite W = E

-- Get the coordinate difference
getIndexDiff :: Direction -> (Int, Int)
getIndexDiff E = (1, 0)
getIndexDiff S = (0, 1)
getIndexDiff W = (-1, 0)
getIndexDiff N = (0, -1)

-- A tile is either a pipe that goes from a direction to another, an empty tile,
-- or the start tile
-- StartUndef is there because we don't know what's the type of the start tile
-- until parsing the input
data Tile = Pipe Direction Direction | Empty | StartUndef | Start Direction Direction deriving (Show, Eq)

-- Can the tile bet accessed from the given direction ?
canAccessFrom :: Direction -> Tile -> Bool
canAccessFrom dir (Pipe dir1 dir2) = dir1 == dir || dir2 == dir
canAccessFrom _ _ = False

-- Get the other end of a pipe tile when given on of its ends
getOtherEnd :: Tile -> Direction -> Direction
getOtherEnd (Pipe dir1 dir2) dir
  | dir1 == dir = dir2
  | dir2 == dir = dir1
  | otherwise = error "Invalid start direction to get other end of tile"
getOtherEnd _ _ = error "Calling 'getOtherEnd' on a non-pipe tile is an error"

-- Returns whether or not the given Pipe or Start tile has a specific direction
hasDir :: Tile -> Direction -> Bool
hasDir (Pipe dir1 dir2) dir = dir == dir1 || dir == dir2
hasDir (Start dir1 dir2) dir = dir == dir1 || dir == dir2
hasDir _ _ = error "Calling 'hasDir' on a non-pipe tile is an error"

-- Returns whether this tile is a start tile
isStart :: Tile -> Bool
isStart StartUndef = True
isStart (Start _ _) = True
isStart _ = False

-- The world is simply an array of tiles
type World = Array (Int, Int) Tile

-- Parse the input string
parseInput :: [String] -> World
parseInput strs =
  let tempWorld = array bounds $ concat $ zipWith parseLine strs [0 ..]
   in fillStart tempWorld
  where
    bounds = ((0, 0), (width - 1, height - 1))
    height = length strs
    width = case strs of
      x : _ -> length x
      [] -> 1
    -- This little function is used to detect which type of pipe the
    -- starting tile is
    fillStart world =
      let startInd = getStartTileInd world
          neighbors = zip (map (getNeighbor world startInd) directions) directions
          accessibleDirs = [d | (Just (_, t), d) <- neighbors, canAccessFrom (opposite d) t]
       in case accessibleDirs of
            [dir1, dir2] -> world // [(startInd, Start dir1 dir2)]
            _ -> error "The start tile should have exactly 2 accessible neighbors"

-- Parse a line of the input
parseLine :: String -> Int -> [((Int, Int), Tile)]
parseLine str j =
  zipWith (\char i -> ((i, j), parseTile char)) str [0 ..]

-- Parse a single tile
parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile 'S' = StartUndef
parseTile '|' = Pipe N S
parseTile '-' = Pipe E W
parseTile 'L' = Pipe N E
parseTile 'J' = Pipe N W
parseTile '7' = Pipe S W
parseTile 'F' = Pipe S E
parseTile c = error $ "Invalid tile: '" ++ [c] ++ "'"

-- Get the neighbor of a tile in a given direction (returns nothing if we exceed
-- the world boundaries)
getNeighbor :: World -> (Int, Int) -> Direction -> Maybe ((Int, Int), Tile)
getNeighbor world (i, j) dir =
  let (_, (width, height)) = bounds world
      (a, b) = getIndexDiff dir
      x = i + a
      y = j + b
   in if x >= 0 && x <= width && y >= 0 && y <= height
        then Just ((x, y), world ! (x, y))
        else Nothing

-- Get the coordinates of the tiles that make up the pipe loop
getPipeLoopPath :: World -> (Int, Int) -> Direction -> [(Int, Int)]
getPipeLoopPath world startInd startDir =
  let firstTileInd = case (getNeighbor world startInd startDir) of
        Just (ind, _) -> ind
        Nothing -> error "Invalid starting index or direction"
   in getPipeLoopPath' firstTileInd (opposite startDir)
  where
    getPipeLoopPath' curInd fromDir
      | curInd == startInd = [curInd]
      | otherwise = let (ind, d) = findNext curInd fromDir in curInd : getPipeLoopPath' ind d
    findNext curInd fromDir =
      let tile = world ! curInd
          dir = getOtherEnd tile fromDir
          neighbor = case getNeighbor world curInd dir of
            Just (ind, _) -> ind
            Nothing -> error $ "No neighbor found for tile at position " ++ (show curInd)
       in (neighbor, opposite dir)

-- Get the number of tiles enclosed by the pipe loop
-- This uses a kind of ray algorithm: go through each line, if we go straight
-- north - south or south - north, switch the status (inside / outside) and
-- count the tiles accordingly
getNumberOfInsideTiles :: World -> [(Int, Int)] -> Int
getNumberOfInsideTiles world loop =
  sum $ map (getNumberOfInsideTilesInLine False False False 0) [0 .. height]
  where
    (_, (width, height)) = bounds world
    getNumberOfInsideTilesInLine inside nSeen sSeen i j
      | i > width = 0
      | (i, j) `elem` loop =
          let newNSeen = nSeen /= hasDir tile N
              newSSeen = sSeen /= hasDir tile S
              (newInside, n, s) =
                if newNSeen && newSSeen
                  then (not inside, False, False)
                  else (inside, newNSeen, newSSeen)
           in getNumberOfInsideTilesInLine newInside n s (i + 1) j
      | otherwise = case inside of
          True -> 1 + getNumberOfInsideTilesInLine inside nSeen sSeen (i + 1) j
          False -> getNumberOfInsideTilesInLine inside nSeen sSeen (i + 1) j
      where
        tile = world ! (i, j)

-- Get the index of the starting tile
getStartTileInd :: World -> (Int, Int)
getStartTileInd world =
  case filter (isStart . snd) $ assocs world of
    [(ind, _)] -> ind
    [] -> error "No start tile found!"
    _ -> error "Multiple start tiles found!"

-- Solution to problem 1: length of the pipe loop divided by 2
solution1 :: World -> Int
solution1 world =
  let startInd = getStartTileInd world
      path = getPipeLoopPath world startInd S
   in length path `div` 2

-- Solution to problem 2: count the number of enclosed tiles
solution2 :: World -> Int
solution2 world =
  let startInd = getStartTileInd world
      path = getPipeLoopPath world startInd S
   in getNumberOfInsideTiles world path

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      world = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 world)
  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 world)
