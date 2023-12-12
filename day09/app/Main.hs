module Main where

-- Generate a sequence of differences between each successive value of the
-- input sequence
generateDiffSeq :: [Int] -> [Int]
generateDiffSeq s@(_ : xs) =
  let pairs = zip s xs
   in [b - a | (a, b) <- pairs]
generateDiffSeq [] = []

-- Predict the next value for a sequence
predictedValue :: [Int] -> Int
predictedValue vals
  -- If all values are 0, then the next value is 0
  | all (== 0) vals = 0
  -- Otherwise, add the predicted value of the diff sequence
  | otherwise = (last vals) + (predictedValue $ generateDiffSeq vals)

parseInput :: [String] -> [[Int]]
parseInput = map (map read) . map words

-- Reverses the input sequences
transformInput :: [[Int]] -> [[Int]]
transformInput = map reverse

-- Sum the predicted values
solution1 :: [[Int]] -> Int
solution1 = sum . map predictedValue

-- For problem 2, we need to simply reverse the input sequences to predict
-- the previous value
solution2 :: [[Int]] -> Int
solution2 = solution1 . transformInput

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      parsedInput = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
