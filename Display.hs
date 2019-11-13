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
      A   B   C   D   E   F   G   H
    +---+---+---+---+---+---+---+---+
  1 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+
    ...
-}
stringify :: Board -> Int -> String -> String
stringify (Board m) cs rs = concat result
    where sep = "  " ++ concat (intersperse "---" (replicate (cs+1) "+"))
          colHeader = "    " ++ concat (intersperse "   " (map (\x -> [x]) rs))
          rows :: [String]
          rows = map (buildRow (Board m)) locs
              where locs :: [[(Char, Int)]]
                    locs = groupBy (\ l1 l2 -> snd l1 == snd l2 ) [(r, c) | c <- [1 .. cs], r <- rs] -- guaranteed to be in order
          result = (intersperse ("\n") ( intersperse sep ([colHeader] ++ rows))) ++ ["\n", sep]

-- Build an individual row based on Board contents
buildRow :: Board -> [(Char, Int)] -> String
buildRow (Board m) locs = show (snd (head locs)) ++ " |" ++ concat (map (\ p -> " " ++ p ++ " |") pieces)
    where pieces = map (\ l -> case Map.lookup l m of
                                 Nothing           -> "E" -- error
                                 Just (c, Nothing) -> " "
                                 Just (c, Just (Piece p _))  -> if p == OneB
                                                                then "B"
                                                                else "W") locs

setCursor :: (Int, Int) -> IO ()
setCursor = undefined
