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

import Model


-- Unicode chars for Checkers pieces.
wMan :: String
wMan = "●"

wQueen :: String
wQueen = "◕"

bMan :: String
bMan = "○"

bQueen :: String
bQueen = "◔"

-- Convert a checkers Piece to String.
pts :: Piece -> String
pts (Piece OneB Man)   = bMan
pts (Piece OneB Queen) = bQueen
pts (Piece TwoW Man)   = wMan
pts (Piece TwoW Queen) = wQueen 


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
buildRow (Board m) locs = show (snd (head locs)) ++ " |" ++ concat (map (\ p -> p ++ "|") pieces)
    where pieces = map (\ l -> case Map.lookup l m of
                                 Nothing           -> "E" -- error
                                 Just (c, Nothing) -> if c == White
                                                      then whiteBG ++ "   " ++ blackBG
                                                      else "   "
                                 Just (c, Just p)  -> if c == White
                                                      then whiteBG ++ " " ++ (pts p) ++ " " ++ blackBG
                                                      else " " ++ (pts p) ++ " ") locs


{- Cursor Control Functionality -}

-- A representation of the ESC char.
esc :: Char
esc = chr 27 -- The 'escape' special character

-- Clear the terminal and set cursor at (0,0)
clear :: String
clear = esc : "[2J"

-- Toggle while background
whiteBG :: String
whiteBG = esc : "[47m"

-- Toggle black background
blackBG :: String
blackBG = esc : "[40m"

-- Position the cursor at given Row, Col coordinates (in screen space).
setCursor :: Int -> Int -> String
setCursor line col = [esc] ++ "[" ++ (show line) ++ ";" ++ (show col) ++ "H"


{- DEBUGGING -}

dump :: Board -> IO ()
dump b = putStrLn (stringify b 8 ['A'..'H'])

dumpEval :: (Maybe Board, Maybe String) -> IO ()
dumpEval (Nothing, Just s) = putStrLn s
dumpEval (Just b, Nothing) = dump b
dumpEval (Just b, Just s)  = do dump b
                                putStrLn s
dumpEval _ = putStrLn "*** ERROR ***"













