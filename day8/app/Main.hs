module Main where

import qualified Data.Map as Map
import Prelude hiding(Left, Right)
import Text.Read

data Instruction = Left | Right deriving(Show)

instance Read Instruction where
  readsPrec d r = 
     case r of
       x:xs -> case x of
                 'L' -> [(Left, xs)]
                 'R' -> [(Right, xs)]
                 _ -> error ""
       [] -> error ""

type Node = String
type Network = Map.Map Node (Node, Node)


main :: IO ()
main = putStrLn "Hello, Haskell!"
