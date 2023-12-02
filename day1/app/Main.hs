import Text.Read
import Data.List

replaceSpelledNums :: String -> String
replaceSpelledNums [] = []
replaceSpelledNums l@(x:xs)
  | "one" `isPrefixOf` l = next '1'
  | "two" `isPrefixOf` l = next '2'
  | "three" `isPrefixOf` l = next '3'
  | "four" `isPrefixOf` l = next '4'
  | "five" `isPrefixOf` l = next '5'
  | "six" `isPrefixOf` l = next '6'
  | "seven" `isPrefixOf` l = next '7'
  | "eight" `isPrefixOf` l = next '8'
  | "nine" `isPrefixOf` l = next '9'
  | otherwise = x:replaceSpelledNums xs
  where next c = c:(l ++ replaceSpelledNums xs)

sumCalibrationValues :: (Num a, Read a) => [String] -> a
sumCalibrationValues input =
  let filteredInput = [[e | e <- l, e `elem` ['0'..'9']] | l <- input]
      calNums = [[x, last l] | l@(x:_) <- filteredInput]
      calVals = [case(readMaybe l) of Just e -> e
                                      Nothing ->
                                        error "couldn't read integer, "
                                        "this shoudln't happen"
                | l@(_:_) <- calNums]
  in sum calVals

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn
    ("Solution to problem 1: " ++ 
      (show (sumCalibrationValues (lines input) :: Integer))
    )
  putStrLn
    ("Solution to problem 2: " ++
      (show (sumCalibrationValues (map replaceSpelledNums (lines input))
      :: Integer))
    )

