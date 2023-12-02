import Text.Read

replaceSpelledNums :: String -> String
replaceSpelledNums [] = []
replaceSpelledNums l@('o':'n':'e':_) = '1':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('t':'w':'o':_) = '2':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('t':'h':'r':'e':'e':_) = '3':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('f':'o':'u':'r':_) = '4':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('f':'i':'v':'e':_) = '5':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('s':'i':'x':_) = '6':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('s':'e':'v':'e':'n':_) = '7':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('e':'i':'g':'h':'t':_) = '8':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums l@('n':'i':'n':'e':_) = '9':(l ++ replaceSpelledNums (drop 1 l))
replaceSpelledNums (h:xs) = h:replaceSpelledNums xs

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

