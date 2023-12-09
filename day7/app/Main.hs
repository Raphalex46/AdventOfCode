module Main where

import Data.List

-- Define newtypes and type aliases
type Card = Int

type Bid = Int

-- We need a newtype for the hand as we will define a custom ordering on hands
newtype Hand = Hand [Card] deriving (Eq, Show)

-- Our custom ordering on hands: if the hands have different types, then
-- apply the ordering of hand types, otherwise, use the lexicographic comparison
-- of the list of card values
instance Ord Hand where
  a@(Hand ca) <= b@(Hand cb)
    | aType /= bType = aType < bType
    | otherwise = ca <= cb
    where
      aType = fromHand a; bType = fromHand b

-- Hand types are defined as an ordered enum
data HandType = HC | P | TP | TOF | FH | FoOK | FiOK deriving (Eq, Ord, Enum, Show, Bounded)

-- Computes the hand type from a hand
fromHand :: Hand -> HandType
fromHand (Hand cs) =
  -- We group cards to get the type of hand
  let cardGroupsJ = map length (group . sort . filter (/= 1) $ cs)
      -- We handle jokers simply by adding them to highest group of matching cards
      numJ = countOcc 1 cs
      cardGroups = case sortBy (flip compare) cardGroupsJ of
        x : xs -> (x + numJ) : xs
        -- If the list is empty, it means we have only jokers (so it's a FiOK)
        [] -> [numJ]
   in -- Check for each element within the list and return the corresponding card type
      -- (a bit tedious but very clean and readable I think)
      if 5 `elem` cardGroups then FiOK else
      if 4 `elem` cardGroups then FoOK else
      -- If we have 3 matching cards, check for the FH
      if 3 `elem` cardGroups then if 2 `elem` cardGroups then FH else TOF else
      -- If we have a pair but no FH, check for two pairs
      if 2 `elem` cardGroups then if (countOcc 2 cardGroups) == 2 then TP else P
      -- Otherwise, the hand is just HC
      else HC
  where
    countOcc n = length . filter (== n)

-- Parse hand from an input string
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
      | c `notElem` ['2' .. '9'] = error $ "Invalid input card value: \"" ++ [c] ++ "\""
      | otherwise = read [c]

-- Parse the input file: hands and their corresponding bids
parseInput :: [String] -> [(Hand, Bid)]
parseInput s =
  let splitStrs = map words s
   in zip (map (parseHand . (!! 0)) splitStrs) (map (read . (!! 1)) splitStrs)

-- Transform hands by changing J values (11) to jokers (1). (For part 2 of the
-- problem)
transformHands :: [Hand] -> [Hand]
transformHands hands =
  map Hand [map (\x -> if x == 11 then 1 else x) cards | Hand cards <- hands]

-- Solution to part 1 of problem
solution1 :: [(Hand, Bid)] -> Int
solution1 hands =
  let sortedHands = sort hands
      sortedBids = map snd sortedHands
   in sum $ zipWith (*) sortedBids [1 .. length sortedBids]

-- Solution to part 2 of problem (same algorithm, but with transformed hands)
solution2 :: [(Hand, Bid)] -> Int
solution2 handsBids =
  let hands = map fst handsBids
      bids = map snd handsBids
      transformedHands = transformHands hands
   in solution1 $ zip transformedHands bids

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines inputStr
      parsedInput = parseInput input
  putStrLn $ "Solution to problem 1: " ++ (show $ solution1 parsedInput)
  putStrLn $ "Solution to problem 2: " ++ (show $ solution2 parsedInput)
