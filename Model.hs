{-
	CS 641: Checkers Project
	Game logic

	@authors: Sergey Goldobin, Justin Lad
	10/31/2019
-}

module Model where

import qualified Data.Map.Strict as Map
import Data.List

-- Data type for two game modes.
data Mode = Classic | Inverse
    deriving (Show, Read, Eq)

-- The rank of a piece on the board.
data Rank = Man | Queen
    deriving (Show, Eq)

data Player = One | Two
    deriving (Show, Eq)

data Piece = Piece Player Rank

data Color = Black | White
    deriving (Show, Eq)

data Location = Location (Char, Int)

data Tile  = Tile Location Color

data Board = Board (Map.Map Tile (Maybe Piece))

-- From (Row, Column) to (Row, Column)
type Move = (Location, Location)

-- Create a new game board of a given dimension.
new :: Int -> Board
new s = undefined

-- Given the current board state, game mode, player, and a player's move, evaluate the move.
evalMove :: Board -> Mode -> Player -> Move -> (Maybe Board, Maybe String)
evalMove = undefined

