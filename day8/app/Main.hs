module Main where

import qualified Data.Map as Map
import Prelude hiding(Left, Right)
import Data.Char

data Instruction = Left | Right deriving(Show)
type Node = String
type NodePath = (Node, (Node, Node))
type Network = Map.Map Node (Node, Node)
type Puzzle = ([Instruction], Network)

parseInstruction :: Char -> Instruction
parseInstruction 'L' = Left
parseInstruction 'R' = Right
parseInstruction c = error $ "Invalid instruction: \"" ++ [c] ++ "\""

parseNodePath :: String -> NodePath
parseNodePath s =
  let filteredStr = map replace s
   in case words filteredStr of
        [n, npl, pnr] -> (n, (npl, pnr))
        _ -> error "Invalid path input"
  where
      replace c
        | isAlpha c || isDigit c || isSpace c = c
        | otherwise = ' '

parseInput :: [String] -> Puzzle
parseInput (s:_:ss) =
  let instrs = map parseInstruction s
      network = foldr insertInNetwork Map.empty $ map parseNodePath ss
    in (instrs, network)
  where
    insertInNetwork (n, (npl, npr)) net =
      Map.insert n (npl, npr) net
parseInput _ = error "Invalid problem input"

followInstruction :: Instruction -> (Node, Node) -> Node
followInstruction Left (n , _) = n
followInstruction Right (_, n) = n

advance :: Network -> [Node] -> Instruction -> [Node]
advance net n instr =
  let nodes = map (net Map.!) n
   in map (followInstruction instr) nodes

getPath :: Network -> [Node] -> (Node -> Bool) -> [Instruction] -> [[Node]]
getPath net cur isEnd (i:is) 
  | all isEnd cur = []
  | otherwise = cur:getPath net (advance net cur i) isEnd is
getPath _ _ _ [] = []

getPathUntilCycle :: Network -> Node -> [Node] -> Node -> [Instruction] -> ([Node], [Instruction])
getPathUntilCycle net cur path lastEndNode (i:is)
  | cur == lastEndNode = (path, is)
  | (last cur) == 'Z' = getPathUntilCycle net next (cur:path) cur is 
  | otherwise = getPathUntilCycle net next (cur:path) lastEndNode is
  where next = head $ advance net [cur] i
getPathUntilCycle _ _ _ _ [] = ([], [])

getCycle :: Network -> Node -> Node -> [Instruction] -> [Node]
getCycle net cur start (i:is)
  | next == start = [cur]
  | otherwise = cur:getCycle net next start is
  where next = head $ advance net [cur] i
getCycle _ _ _ [] = []

solution1 :: Puzzle -> Int
solution1 (instrs, net) =
  length $ getPath net ["AAA"] isEnd (cycle instrs)
    where isEnd = all (== 'Z')

stepsUntilCyclesMeet :: [Int] -> [Int] -> [Int] -> Int
stepsUntilCyclesMeet startPos endPos cyclelengths
  | endPos == startPos = 0
  | otherwise = 1 + stepsUntilCyclesMeet [(a + 1) `mod` b | (a, b) <- zip startPos cyclelengths] endPos cyclelengths
  where step = maximum cyclelengths - minimum startPos

solution2 :: Puzzle -> ([[Node]], [[Node]])
solution2 (instrs, net) =
  let startNodes = filter ((== 'A') . last) $ Map.keys net
      pathsRemInstr = map (\n -> getPathUntilCycle net n [] "ZZZZZZ" (cycle instrs)) startNodes
      cycles = [getCycle net (head paths) (head paths) i | (paths, i) <- pathsRemInstr]
  in (map (drop 1) $ fst (unzip pathsRemInstr), cycles)

-- We need to to compute the product of the cycle lengths minus the time it takes to get to the initial start from the 0 point
main :: IO ()
main = do
  inputStr <- readFile "example2.txt"
  let input = lines inputStr 
      parsedInput@(instr, net) = parseInput input
      (paths, cycles) = solution2 parsedInput
      max = maximum $ map length paths
      start = [((-a) + max) `mod` b | (a, b) <- zip (map length paths) (map length cycles)]
      cyclelengths = map length $ map (reverse . reverse . (dropWhile ((/= 'Z') . last))) $ cycles
  putStrLn $ show parsedInput
  putStrLn $ show $ filter ((== 'A') . last) $ Map.keys (snd parsedInput)
--  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
--  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
  putStrLn $ show $  solution2 parsedInput
  putStrLn $ show $ cyclelengths
  putStrLn $ show $ map length paths
  putStrLn $ show $ start
  putStrLn $ show $ max
  putStrLn $ show $ stepsUntilCyclesMeet (replicate (length start) 0) start cyclelengths
