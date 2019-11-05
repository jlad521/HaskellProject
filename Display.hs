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
--import Text.Show.Unicode TODO fogure out how to install a Haskell package

import Model

{- Generate a string representation of the board,
      1   2   3   4   5   6   7   8
    +---+---+---+---+---+---+---+---+
  A |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+
    ...
-}
stringify :: Board -> Int -> String -> String
stringify (Board m) cs rs = []
    where sep = concat (intersperse "---" (replicate (cs+1) "+"))
          colHeader = "  " ++ concat (intersperse "    " (map show [1 .. cs]))
          rows :: [String]
          rows = map (buildRow (Board m)) locs
              where locs :: [[(Char, Int)]]
                    locs = groupBy (\ l1 l2 -> fst l1 == fst l2 ) [(r, c) | r <-rs, c <- [1 .. cs]] -- guaranteed to be in order



interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x]++[y]) xs ys)


buildRow :: Board -> [(Char, Int)] -> String
buildRow (Board m) locs = "|" ++ concat (map (\ p -> " " ++ p ++ " |") pieces)
    where pieces = map (\ l -> case Map.lookup l m of
                                 Nothing           -> " "
                                 Just (c, Nothing) -> " "
                                 Just (c, Just (Piece p _))  -> if p == OneB
                                                                then "B"
                                                                else "W") locs
