{-
	CS 641: Checkers Project
	Main driver for the program.

	@authors: Sergey Goldobin, Justin Lad
	10/31/2019
-}

module Checkers where

import System.Environment
import System.IO
import Data.Char
import Data.Maybe
import Text.Read

import Display
import Model



-- Board size hardcoded for now, might be customizeable later.
boardSize :: Int
boardSize = 8

boardChars :: [Char]
boardChars = map chr [ord('A') .. ord('A') + boardSize - 1]

usage :: String
usage = "Usage: runhaskell Checkers {classic|inverse}"

-- Parse command line arguments to the checkers game.
parseArgs :: [String] -> Maybe (GameMode, Int)
parseArgs [m, s] = do mv <- case map toLower m of
                              "classic" -> return Standard
                              "inverse" -> return Inverse
                              _         -> Nothing
                      sv <- case readMaybe s :: Maybe Int of
                              Just i -> return i
                              _      -> Nothing
                      return (mv, sv)
parseArgs _ = Nothing

-- Parse command line arguments and dispatch the appropriate game loop.
main :: IO ()
main = do args <- getArgs
          case parseArgs (args ++ [show boardSize]) of -- hack until board is dynamic
            Nothing           -> putStrLn usage
            Just (mode, size) -> do board <- return actual_board
                                    putStrLn ("Welcome to " ++ (show mode) ++ " Checkers!")
                                    play mode board OneB -- Game always starts with player One

          
-- Parse a given string into a move.
-- Expected move format: RowColumn-RowColumn
-- Shoutout to pattern matching for making this possible.
parseMove :: String -> Maybe Move
parseMove [rs1, cs1, '-', rs2, cs2] =
    do c1 <- if isDigit cs1 then Just (digitToInt cs1) else Nothing
       r1 <- if isLetter rs1 && (toUpper rs1) `elem` boardChars then Just (toUpper rs1) else Nothing
       l1 <- if c1 > 0 && c1 <= boardSize then Just (r1, c1) else Nothing
       c2 <- if isDigit cs2 then Just (digitToInt cs2) else Nothing
       r2 <- if isLetter rs2 && (toUpper rs2) `elem` boardChars then Just (toUpper rs2) else Nothing
       l2 <- if c2 > 0 && c2 <= boardSize then Just (r2, c2) else Nothing
       Just (l1, l2)
parseMove _ = Nothing

-- Get next player to take a turn.
nextPlayer :: Player -> Player
nextPlayer p = if p == OneB then TwoW else OneB


-- Execute the read-eval-print loop for the game.
play :: GameMode -> Board -> Player -> IO ()
play mode b@(Board mp) p =
    do putStrLn (stringify b boardSize boardChars)
       putStrLn ("\nPlayer " ++ (show p) ++ "'s turn.")
       putStrLn ("Enter your move: ")
       mvs <- getLine
       case  parseMove mvs of
         Nothing   -> do putStrLn ("Error parsing the move. Please use the following format: RowColumn-RowColumn, i.e. A1-C3")
                         play mode b p
         Just move -> do case evalMove b p move mode of
                           (Nothing, Just err) -> do putStrLn ("Invalid move: " ++ err)
                                                     play mode b p
                           (Just nb, Just vm)  -> do putStrLn (stringify nb boardSize boardChars)
                                                     putStrLn ("Victory for Player " ++ (show p))
                           (Just nb, Nothing)  -> play mode nb (nextPlayer p)
                           _                   -> putStrLn ("An invalid evaluaton state has occurred.")
