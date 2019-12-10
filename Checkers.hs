{-
	CS 641: Checkers Project
	Main driver for the program.

	@authors: Sergey Goldobin, Justin Lad
	10/31/2019
-}

module Checkers where

import System.Environment
import System.IO
import System.Exit
import Control.Concurrent
import Data.Char
import Data.Maybe
import Text.Read
import Data.List

import Display
import Model



-- Board size hardcoded for now, might be customizeable later.
boardSize :: Int
boardSize = 8

boardChars :: [Char]
boardChars = map chr [ord('A') .. ord('A') + boardSize - 1]

usage :: String
usage = "Usage: runhaskell Checkers {classic|inverse} [auto]"

-- Parse command line arguments to the checkers game.
parseArgs :: [String] -> Maybe (GameMode, Bool, Int)
parseArgs [m, s] = 
    do mv <- case map toLower m of
               "classic" -> return Standard
               "inverse" -> return Inverse
               _         -> Nothing
       sv <- case readMaybe s :: Maybe Int of
               Just i -> return i
               _      -> Nothing
       return (mv, False, sv)
parseArgs [m, ap, s] =
    case parseArgs [m, s] of
      Nothing -> Nothing
      Just (gm, _, bs) -> Just (gm, (ap == "auto"), bs)
parseArgs _ = Nothing

-- Parse command line arguments and dispatch the appropriate game loop.
main :: IO ()
main = do args <- getArgs
          case parseArgs (args ++ [show boardSize]) of -- hack until board is dynamic
            Nothing               -> putStrLn usage
            Just (mode, ap, size) -> do board <- return actual_board
                                        putStrLn ("Welcome to " ++ (show mode) ++ " Checkers!")
                                        moves <- if ap 
                                                 then do putStrLn "Auto-Playing a round using STDIN." 
                                                         readAutoMoves
                                                 else return Nothing
                                        putStrLn clear
                                        putStrLn (setBoard ++ (stringify actual_board 8 ['A'..'H']))
                                        play mode board TwoW moves -- Game always starts with White

          
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

{-
    Get all lines from STDIN.
    If any were read, hand them over to the game loop.
    Used by the autoplay feature.
-}
readAutoMoves :: IO (Maybe [String])
readAutoMoves = do lines <- getContents
                   case lines of
                     "" -> return Nothing
                     ls -> return (Just (filter (/="") (splitOn '\n' lines)))

-- Get next player to take a turn.
nextPlayer :: Player -> Player
nextPlayer p = if p == OneB then TwoW else OneB


-- Execute the read-eval-print loop for the game.
play :: GameMode -> Board -> Player -> Maybe [String] -> IO ()       
play mode b@(Board mp) p mvs =
    do putStr (setContext ("Player " ++ (show p) ++ "'s turn. Enter your move: "))
       putStr setMove
       m <- case mvs of
               Nothing     -> getLine
               Just (x:xs) -> do putStrLn (setError ("Interpreting auto-move " ++ x))
                                 threadDelay 1000000
                                 return x
               Just []     -> do putStrLn (setError ("Auto-Play over, ran out of moves. Shutting down..."))
                                 exitWith ExitSuccess
       fm <- case mvs of
                Nothing     -> return Nothing
                Just []     -> return (Just [])
                Just (_:xs) -> return (Just xs)
       case  parseMove (trim m) of
         Nothing   -> do putStrLn (setError("PARSE ERROR. Please use the following format: RowCol-RowCol, i.e. A1-C3"))
                         play mode b p Nothing -- Errors in auto, switch to manual
         Just move -> do case evalMove b p move mode of
                           (Nothing, Just err) -> do putStr (setError ("Invalid move: " ++ err))
                                                     play mode b p Nothing -- Invalid moves in auto switch to manual
                           (Just nb, Just vm)  -> do putStr (stringify nb boardSize boardChars)
                                                     putStr (setContext ("Victory for Player " ++ (show p)))
                           (Just nb, Nothing)  -> do putStr (refreshBoard nb)
                                                     putStr (setError "")
                                                     play mode nb (nextPlayer p) fm 
                           _                   -> putStrLn (setError ("An invalid evaluaton state has occurred."))





{- Helper functions-}

-- Split a list on a given delimeter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim lst = foldr f [[]] lst
  where f c l@(x:xs) | c == delim = [] : l
                     | otherwise = (c : x) : xs

trim :: String -> String
trim s = filter (not.isSpace) s















