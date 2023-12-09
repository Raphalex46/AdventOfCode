module Main where

import Data.List

type Card = Int
type Bid = Int
newtype Hand = Hand [Card] deriving(Eq, Show)

data HandType = HC | P | TP | TOF | FH | FoOK | FiOK deriving(Eq, Ord, Enum, Show, Bounded)

fromHand :: Hand -> HandType
fromHand (Hand cs) = 
  let cardGroups = map length (group . sort $ cs)
   in if 5 `elem` cardGroups then FiOK else
      if 4 `elem` cardGroups then FoOK else
      if 3 `elem` cardGroups then if 2 `elem` cardGroups then FH else TOF else
      if 2 `elem` cardGroups then if (countOcc 2 cardGroups) == 2 then TP else P
      else HC
  where
    countOcc n = length . filter (==n)

parseHand :: String -> Hand
parseHand s
  | length s /= 5 = error "Invalid input hand (hands must have exactly 5 cards)"
  | otherwise = Hand $ map parseCard s
  where
    parseCard 'A' = 14
    parseCard 'K' = 13
    parseCard 'Q' = 12
    parseCard 'J' = 11
    parseCard 'T' = 10
    parseCard c
      | c `notElem` ['2'..'9'] = error $ "Invalid input card value: \"" ++ [c] ++ "\""
      | otherwise = read [c]

parseInput :: [String] -> [(Hand, Bid)]
parseInput s =
  let splitStrs = map words s
   in zip (map (parseHand . (!! 0)) splitStrs) (map (read . (!! 1)) splitStrs)

solution1 :: [(Hand, Bid)] -> Int
solution1 hands =
  let sortedHands = sort hands
      sortedBids = map snd sortedHands
  in sum $ zipWith (*) sortedBids [1..length sortedBids]

instance Ord Hand where
  a@(Hand ca) <= b@(Hand cb)
    | aType /= bType = aType < bType
    | otherwise = ca <= cb
    where aType = fromHand a; bType = fromHand b


main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      parsedInput = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
