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

{-
	Convert a Piece object to String.
-}
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

{-
	Build an individual row based on Board contents
-}
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


{- #### Cursor Control Functionality #### -}

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

{-
	Position the cursor at given Row, Col coordinates (in screen space).
-}
setCursor :: Int -> Int -> String
setCursor line col = [esc] ++ "[" ++ (show line) ++ ";" ++ (show col) ++ "H"

{-
	Generate an escape sequence to position a given value at a given location.
-}
setTile :: Maybe Piece -> Loc -> String
setTile p (col, row) = (setCursor (1 + (2 * row)) (1 + (4 * colOffset))) ++ toPut
    where colOffset = (ord col) - 64
          toPut = case p of
                    Nothing    -> " "
                    Just piece -> pts piece

-- Generates a line of 80 space chars
clearLine :: String
clearLine = take 80 (repeat ' ')

{-
	Set the contents of the contextual message area
-}
setContext :: String -> String
setContext l = (setCursor 20 0) ++ clearLine ++ (setCursor 20 0) ++ l

{-
	Set the contents of the error message area.
-}
setError :: String -> String
setError e = (setCursor 21 0) ++ clearLine ++ (setCursor 21 0) ++ e

-- Switch the cursor to move input area
setMove :: String 
setMove = (setCursor 22 0) ++ clearLine ++ (setCursor 22 0)

-- Switch the cursor to board area.
setBoard :: String
setBoard = setCursor 0 0

-- Switch the cursor to help area.
setHelp :: String
setHelp = setCursor 26 0

{-
	Perform an in-place update for all game tiles on the board.
-}
refreshBoard :: Board -> String
refreshBoard b = concat (map func options)
    where options = [(c, r) | c <- ['A'..'H'], r <- [1..8]]
          func loc = case getTile b loc of
                        (White, _) -> ""
                        (Black, p) -> setTile p loc
                        


{- #### DEBUGGING UTILITIES #### -}

dump :: Board -> IO ()
dump b = putStrLn (stringify b 8 ['A'..'H'])

dumpEval :: (Maybe Board, Maybe String) -> IO ()
dumpEval (Nothing, Just s) = putStrLn s
dumpEval (Just b, Nothing) = dump b
dumpEval (Just b, Just s)  = do dump b
                                putStrLn s
dumpEval _ = putStrLn "*** ERROR ***"













