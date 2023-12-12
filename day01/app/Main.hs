import Data.List
import Text.Read

-- Replaces spelled out numbers in the text with actual numbers
replaceSpelledNums :: String -> String
replaceSpelledNums [] = []
replaceSpelledNums l@(x : xs)
  | "one" `isPrefixOf` l = next '1'
  | "two" `isPrefixOf` l = next '2'
  | "three" `isPrefixOf` l = next '3'
  | "four" `isPrefixOf` l = next '4'
  | "five" `isPrefixOf` l = next '5'
  | "six" `isPrefixOf` l = next '6'
  | "seven" `isPrefixOf` l = next '7'
  | "eight" `isPrefixOf` l = next '8'
  | "nine" `isPrefixOf` l = next '9'
  | otherwise = x : replaceSpelledNums xs
  where
    -- be careful not to remove the entire matched word and only advance
    -- one character at a time: in a case like "oneight", we want to actually
    -- consider both the '1' and the '8'
    next c = c : (l ++ replaceSpelledNums xs)

-- Match and sum the calibration values
sumCalibrationValues :: (Num a, Read a) => [String] -> a
sumCalibrationValues input =
  -- filter for digits
  let filteredInput = [[e | e <- l, e `elem` ['0' .. '9']] | l <- input]
      -- get the first and last digits (or double the single digit if there is
      -- a single digit
      calNums = [[x, last l] | l@(x : _) <- filteredInput]
      calVals =
        [ case (readMaybe l) of
            Just e -> e
            Nothing ->
              error
                "couldn't read integer, "
                "this shoudln't happen"
          | l@(_ : _) <- calNums
        ]
   in sum calVals

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn
    ( "Solution to problem 1: "
        ++ (show (sumCalibrationValues (lines input) :: Integer))
    )
  putStrLn
    ( "Solution to problem 2: "
        ++ ( show
               ( sumCalibrationValues (map replaceSpelledNums (lines input)) ::
                   Integer
               )
           )
    )
