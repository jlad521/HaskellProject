
module Model where

import Data.Char -- import ord fn 
import qualified Data.Map.Strict as Map

data Board  = Board (Map.Map (Char, Int) (Color, Maybe Piece))
    deriving (Show, Eq)
--data Tile   = Location (Char, Int) Color
data Color  = Black | White 
    deriving (Show, Eq)
data Piece  = Piece Player Rank
    deriving (Show, Eq)
data Rank   = Man | Queen
    deriving (Show, Eq)
data Player = OneB | TwoW
    deriving (Show, Eq)

type Loc = (Char, Int)

type Move = (Loc, Loc)

new :: Board
new = Board (Map.fromList([ ((c, i), (White, Nothing)) | c <- ['A' .. 'H'], i <- [1..8]]))

b = new
p1 = (Black, Just(Piece OneB Man))
p2 = (Black, Just(Piece TwoW Man))
actual_board = addColor b 

updateBoard :: Move -> Board -> Board 
updateBoard (ol, dl) (Board brd) = nb
    where tile :: Maybe (Color, Maybe Piece)
          tile = Map.lookup ol brd -- look up tile of origin
          nb   =  case tile of
                    Just (c, Just p) -> Board (Map.adjust (\ tup -> (fst tup, Just p)) dl am)       -- Move piece from OL to DL
                                          where am = Map.adjust (\ tup -> (fst tup, Nothing)) ol brd -- update OL to nothing
                    _                -> Board brd -- Something went wrong, cancel update.
         

addColor :: Board -> Board
addColor (Board pos) = aux (Board pos) 'H' 8

    where aux (Board brd) 'A' 1 = Board (Map.insert ('A',1) (Black, Just(Piece OneB Man)) brd)
          aux (Board brd)  c  row 

                          | ord c < ord 'A' = aux (Board brd) 'H' (row -1)

                          | row >= 6 = if row `mod` 2 == 0 && ord c `mod` 2 == 0 then white_piece  
                                       else if row `mod` 2 == 1 && ord c `mod` 2 == 1 then white_piece 
                                       else next
                          
                          | row == 4 = if ord c `mod` 2 == 0 then empty_square
                                       else next
                          | row == 5 = if ord c `mod` 2 == 1 then empty_square
                                       else next 

                          | row <= 3 = if ord c `mod` 2 == 1 && row `mod` 2 == 1 then black_piece
                                       else if ord c `mod` 2 == 0 && row `mod` 2 == 0 then black_piece
                                       else next 
                          
                          | otherwise = aux (Board brd) 'A' 1 

                where black_piece  = aux (Board (Map.insert (c,row) (Black, Just(Piece OneB Man)) brd )) (pred c) row  
                      white_piece  = aux (Board (Map.insert (c,row) (Black, Just(Piece TwoW Man)) brd )) (pred c) row  
                      empty_square = aux (Board (Map.insert (c,row) (Black, Nothing ) brd )) (pred c) row 
                      next         = aux (Board brd) (pred c) row

isWin :: Board -> Player -> Bool
isWin = undefined

isTie :: Board -> Bool
isTie = undefined

-- TEMP implementation for testing:
-- do no validation, just apply the move.
evalMove :: Board -> Player -> Move -> (Maybe Board, Maybe String)
evalMove b p m = (Just (updateBoard m b), Nothing)
