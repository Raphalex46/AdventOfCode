import Data.Char
import Data.List.Split

type CubeSet = (Int, Int, Int) -- Corrsponds to red, green, blue

type Game = (Int, [CubeSet]) -- Id and sets

-- Parses a game id
-- we suppose the input is only the "Game x" part of the input line
parseGameId :: String -> Int
parseGameId s =
  read (filter isDigit s)

-- Parses a single set of cubes
-- we suppose the input consists only of a set of cubes delimited by commas and
-- spaces
parseCubeSet :: String -> CubeSet
parseCubeSet s =
  let -- split input at whitespace
      w = words ([x | x <- s, x /= ','])
      -- this might be a bit verbose, but the number of cubes is found at even
      -- indices and the color of the cubes at odd indices
      nums = [w !! i | i <- [0 .. (length w - 1)], even i]
      colors = [w !! i | i <- [1 .. (length w - 1)], odd i]
      -- cubes are not sorted by color in the input so we have to check like
      -- this. This is probably not the best way to write it but it works
      assoc = zip nums colors
      r = case [read v | (v, c) <- assoc, c == "red"] of v : _ -> v; [] -> 0
      g = case [read v | (v, c) <- assoc, c == "green"] of v : _ -> v; [] -> 0
      b = case [read v | (v, c) <- assoc, c == "blue"] of v : _ -> v; [] -> 0
   in (r, g, b)

-- Parse a game (returns the game id and the list of associated cube sets)
parseGame :: String -> Game
parseGame s =
  case (splitOn ":" s) of
    [gamePart, setsPart] ->
      let gameId = parseGameId gamePart
          sets = map parseCubeSet (splitOn ";" setsPart)
       in (gameId, sets)
    _ -> error "the input should contain 2 parts separated by a single ':'"

-- Checks if a set of cubes is possible given a maximum number of each colored
-- cubes
setIsPossible :: CubeSet -> Int -> Int -> Int -> Bool
setIsPossible (r, g, b) mr mg mb =
  r <= mr && g <= mg && b <= mb

-- Checks if a game is possible given a maximum number of each colored cubes
gameIsPossible :: Game -> Int -> Int -> Int -> Bool
gameIsPossible (_, sets) mr mb mg =
  and (map setIsPossible' sets)
  where
    setIsPossible' set = setIsPossible set mr mb mg

-- Computes the power of a game
powerOfMinimumCubes :: Game -> Int
powerOfMinimumCubes (_, set) =
  -- get the minimum amount of cubes of each color for the game to be possible
  -- (this is the maximum number of cubes shown for each color)
  let mr = maximum [r | (r, _, _) <- set]
      mg = maximum [g | (_, g, _) <- set]
      mb = maximum [b | (_, _, b) <- set]
   in -- then return the power
      mr * mg * mb

main :: IO ()
main = do
  input <- readFile "input.txt"
  let games = map parseGame (lines input)
      possibleGames = [gameId | g@(gameId, _) <- games, gameIsPossible g 12 13 14]
      powers = map powerOfMinimumCubes games
  putStrLn ("Solution to problem 1: " ++ (show (sum possibleGames)))
  putStrLn ("Solution to problem 2: " ++ (show (sum powers)))
