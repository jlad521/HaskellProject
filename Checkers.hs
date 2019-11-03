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

usage :: String
usage = "Usage: Checkers {classic|inverse}"

-- Parse command line arguments to the checkers game.
parseArgs :: [String] -> Maybe (Mode, Int)
parseArgs [m, s] = do mv <- case map toLower m of
                              "classic" -> return Classic
                              "inverse" -> return Inverse
                              _         -> Nothing
                      sv <- case readMaybe s :: Maybe Int of
                              Just i -> return i
                              _      -> Nothing
                      return (mv, sv)

-- Parse command line arguments and dispatch the appropriate game loop.
main :: IO ()
main = do args <- getArgs
          case parseArgs (args ++ [show boardSize]) of -- hack until board is dynamic
            Nothing           -> putStrLn usage
            Just (mode, size) -> do board <- return (new size)
                                    putStrLn ("Welcome to " ++ (show mode) ++ " Checkers!")
                                    play mode board One -- Game always starts with player One
          
-- Parse a given string into a move.
-- Expected move format: RowColumn-RowColumn
parseMove :: String -> Maybe Move
parseMove = undefined

-- Get next player to take a turn.
nextPlayer :: Player -> Player
nextPlayer p = if p == One then Two else One


-- Execute the read-eval-print loop for the game.
play :: Mode -> Board -> Player -> IO ()
play mode b@(Board mp) p =
    do putStrLn (stringify b)
       putStrLn ("\nPlayer " ++ (show p) ++ "'s turn.")
       putStrLn ("Enter your move: ")
       mvs <- getLine
       case  parseMove mvs of
         Nothing   -> do putStrLn ("Error parsing the move. Please use the following format: RowColumn-RowColumn, i.e. A1-C3")
                         play mode b p
         Just move -> do case evalMove b mode p move of
                           (Nothing, Just err) -> do putStrLn ("Invalid move: " ++ err)
                                                     play mode b p
                           (Just nb, Just vm)  -> do putStrLn (stringify nb)
                                                     putStrLn ("Victory for Player " ++ (show p))
                           (Just nb, Nothing)  -> play mode nb (nextPlayer p)
                           _                   -> putStrLn ("An invalid evaluaton state has occurred.")
