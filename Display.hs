{-
	CS 641: Checkers Project
	Game visual output controls.

	@authors: Sergey Goldobin, Justin Lad
	10/31/2019
-}

module Display where

import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import System.IO
--import Data.String.Unicode
--import Text.Show.Unicode TODO fogure out how to install a Haskell package

import Model

man :: String
man = "\\26C0"

{- Generate a string representation of the board,
      1   2   3   4   5   6   7   8
    +---+---+---+---+---+---+---+---+
  A |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+
    ...
-}
stringify :: Board -> Int -> String -> String
stringify (Board m) cs rs = concat result
    where sep = "  " ++ concat (intersperse "---" (replicate (cs+1) "+"))
          colHeader = "    " ++ concat (intersperse "   " (map show [1 .. cs]))
          rows :: [String]
          rows = map (buildRow (Board m)) locs
              where locs :: [[(Char, Int)]]
                    locs = groupBy (\ l1 l2 -> fst l1 == fst l2 ) [(r, c) | r <-rs, c <- [1 .. cs]] -- guaranteed to be in order
          result = (intersperse ("\n") ( intersperse sep ([colHeader] ++ rows))) ++ ["\n", sep]

-- Build an individual row based on Board contents
buildRow :: Board -> [(Char, Int)] -> String
buildRow (Board m) locs = [fst (head locs)] ++ " |" ++ concat (map (\ p -> " " ++ p ++ " |") pieces)
    where pieces = map (\ l -> case Map.lookup l m of
                                 Nothing           -> "E" -- error
                                 Just (c, Nothing) -> " "
                                 Just (c, Just (Piece p _))  -> if p == OneB
                                                                then "B"
                                                                else "W") locs

setCursor :: (Int, Int) -> IO ()
setCursor = undeifned
