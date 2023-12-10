module Main where

import qualified Data.Map as Map
import Prelude hiding(Left, Right)
import Data.Char

data Instruction = Left | Right deriving(Show, Eq)
type InstructionStream = ([Instruction], Int)
type Node = String
type NodePath = (Node, (Node, Node))
type Network = Map.Map Node (Node, Node)
type Puzzle = ([Instruction], Network)

getCurInstr :: InstructionStream -> Instruction
getCurInstr (instr, id) = instr !! id

getCurInstrId :: InstructionStream -> Int
getCurInstrId (_, id) = id

advanceStream :: InstructionStream -> InstructionStream
advanceStream (instr, id) = (instr, (id + 1) `mod` length instr)

fromInstrList :: [Instruction] -> InstructionStream
fromInstrList instr = (instr, 0)

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

advance :: Network -> Node -> Instruction -> Node
advance net n instr = followInstruction instr (net Map.! n)

getPath :: Network -> Node -> (Node -> Bool) -> [Instruction] -> [Node]
getPath net cur isEnd (i:is) 
  | isEnd cur = []
  | otherwise = cur:getPath net (advance net cur i) isEnd is
getPath _ _ _ [] = []

getPathUntilCycle :: Network -> Node -> [(Node, Int)] -> InstructionStream -> [(Node, Int)]
getPathUntilCycle net cur path instS
  | (cur, getCurInstrId instS) `elem` path = path
  | otherwise = getPathUntilCycle net next ((cur, getCurInstrId instS):path) (advanceStream instS)
  where next = advance net cur (getCurInstr instS)

getCycle :: Network -> Node -> (Node, Int) -> InstructionStream -> [Node]
getCycle net cur start instrS
  | (next, getCurInstrId $ advanceStream instrS) == start = [cur]
  | otherwise = cur:getCycle net next start (advanceStream instrS)
  where next = advance net cur (getCurInstr instrS)

solution1 :: Puzzle -> Int
solution1 (instrs, net) =
  length $ getPath net "AAA" isEnd (cycle instrs)
    where isEnd = all (== 'Z')

stepsUntilCyclesMeet :: [Int] -> [Int] -> [Int] -> Int
stepsUntilCyclesMeet startPos endPos cyclelengths
  | endPos == startPos = 0
  | otherwise = 1 + stepsUntilCyclesMeet [(a + 1) `mod` b | (a, b) <- zip startPos cyclelengths] endPos cyclelengths
  where step = maximum cyclelengths - minimum startPos

solution2 :: Puzzle -> [[(Node, Int)]]
solution2 (instrs, net) =
  let startNodes = filter ((== 'A') . last) $ Map.keys net
      stream = fromInstrList instrs
   in map (\n -> getPathUntilCycle net n [] stream) startNodes

-- We need to to compute the product of the cycle lengths minus the time it takes to get to the initial start from the 0 point
main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr 
      parsedInput@(instr, net) = parseInput input
      paths = solution2 parsedInput
      lasts = map head paths
      cycles = map (\n -> getCycle net (fst n) n (instr, snd n)) lasts
--      cyclelengths = map length $ map (reverse . reverse . (dropWhile ((/= 'Z') . last))) $ cycles
--  putStrLn $ show parsedInput
--  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
--  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
--  putStrLn $ show $ solution2 parsedInput
  putStrLn $ show $ map length paths
  putStrLn $ show $ map length cycles
  putStrLn $ show $ map (filter ((=='Z') . last)) cycles
