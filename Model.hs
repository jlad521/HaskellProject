
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

data GameMode = Standard | Inverse
    deriving (Show, Eq)


{-
Test function area
-}

--b = new
p1 = (Black, Just(Piece OneB Man))
p2 = (Black, Just(Piece TwoW Man))
actual_board = newBoard

test_valid = updateBoard (('B',6),('B',4)) actual_board

valid_move = validEnd test_valid (('A',3),('C',5)) OneB
invalid_move = validEnd test_valid (('A',3),('B',4)) OneB

test_move = (('H',6),('G',5))
test_start = ('H',6)

possibleHopTest = possibleHop test_valid ('A',3) OneB

{-
End
-}


newBoard :: Board
newBoard = addPieces (Board (Map.fromList([ ((c, i), (White, Nothing)) | c <- ['A' .. 'H'], i <- [1..8]])))

--define these as global, for use in various functions
black_piece = Just (Black, Just(Piece OneB Man))
white_piece = Just (Black, Just(Piece TwoW Man))
black_king  = Just (Black, Just(Piece OneB Queen))
white_king  = Just (Black, Just(Piece TwoW Queen))

addPieces :: Board -> Board
addPieces (Board pos) = aux (Board pos) 'H' 8

    where aux (Board brd) 'A' 1 = Board (Map.insert ('A',1) (Black, Just(Piece OneB Man)) brd)
          aux (Board brd)  c  row 

                    | ord c < ord 'A' = aux (Board brd) 'H' (row -1)
                    | row >= 6 = if row `mod` 2 == 0      && ord c `mod` 2 == 0 then white_piece  
                                 else if row `mod` 2 == 1 && ord c `mod` 2 == 1 then white_piece 
                                 else next
                    | row == 4 = if ord c `mod` 2 == 0 then empty_square
                                 else next
                    | row == 5 = if ord c `mod` 2 == 1 then empty_square
                                 else next 
                    | row <= 3 = if ord c `mod` 2 == 1      && row `mod` 2 == 1 then black_piece
                                 else if ord c `mod` 2 == 0 && row `mod` 2 == 0 then black_piece
                                 else next    
                    | otherwise = aux (Board brd) 'A' 1 

                where black_piece  = aux (Board (Map.insert (c,row) (Black, Just(Piece OneB Man)) brd )) (pred c) row  
                      white_piece  = aux (Board (Map.insert (c,row) (Black, Just(Piece TwoW Man)) brd )) (pred c) row  
                      empty_square = aux (Board (Map.insert (c,row) (Black, Nothing ) brd )) (pred c) row 
                      next         = aux (Board brd) (pred c) row

updateBoard :: Move -> Board -> Board 
updateBoard (ol, dl) (Board brd) = nb
    where tile :: Maybe (Color, Maybe Piece)
          tile = Map.lookup ol brd -- look up tile of origin
          nb   =  case tile of
                    Just (c, Just p) -> Board (Map.adjust (\ tup -> (fst tup, Just p)) dl am)       -- Move piece from OL to DL
                                          where am = Map.adjust (\ tup -> (fst tup, Nothing)) ol brd -- update OL to nothing
                    _                -> Board brd -- Something went wrong, cancel update.

getPiece :: Board -> Loc -> Maybe Piece
getPiece (Board b) l = case Map.lookup l b of
                            Nothing -> Nothing
                            Just (c, Nothing) -> Nothing
                            Just (c, Just p) -> Just p

getRank :: Board -> Loc -> Maybe Rank
getRank (Board b) l = case Map.lookup l b of
                            Nothing -> Nothing
                            Just (c, Nothing) -> Nothing
                            Just (c, Just (Piece player rank)) -> Just rank

removePiece :: Board -> Loc -> Board
removePiece (Board b) (c,i) = (Board (Map.insert (c,i) (Black, Nothing) b))

noMovesLeft :: Board -> Player -> Bool
noMovesLeft (Board b) p = aux (Map.toList b) p 
  where aux [] _      = True 
        aux (t:ts) p' = ( possibleHop (Board b) l p || possibleAdjacent (Board b) l p ) && aux ts p'
            where l  = fst t 

noPiecesLeft :: Board -> Player -> GameMode -> Bool 
noPiecesLeft (Board b) p m = aux (Map.toList b) p m 
  where aux []     _ _  = True  
        aux (t:ts) p' m'  = if snd t == valid_man || snd t == valid_queen then False
                          else aux ts p' m'

        player_chk = if m == Standard && p == OneB then TwoW
                     else if m == Standard && p == TwoW then OneB
                     else p 
        valid_man   =  (Black, Just(Piece player_chk Man))
        valid_queen =  (Black, Just(Piece player_chk Queen)) 

--handles inverse game mode, no need for inverting logic when called in inverse mode
isWin :: Board -> Player -> GameMode -> Bool
isWin (Board b) p m = noPiecesLeft (Board b) p m || noMovesLeft (Board b) p


--Hmm... this is tricky to implement, see http://www.darkfish.com/checkers/rules.html
isTie :: Board -> Bool
isTie = undefined


possibleAdjacent :: Board -> Loc -> Player -> Bool
possibleAdjacent (Board b) sl@(sl_r, sl_c) p=
        case getPiece (Board b) sl of 
             Nothing -> False
             Just (Piece player rank) ->
               case (player, rank) of 
                  (OneB, Man) -> if adjacent (Board b) sl nw Man p   || adjacent (Board b) sl ne Man p then True else False
                  (TwoW, Man) -> if adjacent (Board b) sl sw Man p   || adjacent (Board b) sl se Man p then True else False
                  (_, Queen)  -> if adjacent (Board b) sl nw Queen p || adjacent (Board b) sl ne Queen p 
                                 || adjacent (Board b) sl sw Queen p || adjacent (Board b) sl se Queen p then True else False
  where 
        sw = (pred sl_r, sl_c -1)
        se = (succ sl_r, sl_c -1)
        nw = (pred sl_r, sl_c +1)
        ne = (succ sl_r, sl_c +1)



possibleHop :: Board -> Loc -> Player -> Bool
possibleHop (Board b) sl@(sl_r, sl_c) p = 
        case getPiece (Board b) sl of
          Nothing -> False
          Just (Piece player rank) -> 
             case (player, rank) of 
                (OneB, Man)   -> if fst (validHop (Board b) sl nnww rank p) || fst (validHop (Board b) sl nnee rank p) then True else False
                (TwoW, Man)   -> if fst (validHop (Board b) sl ssww rank p) || fst (validHop (Board b) sl ssee rank p) then True else False
                (_, Queen)    -> if fst (validHop (Board b) sl nnww rank p) || fst (validHop (Board b) sl nnee rank p)
                                 || fst (validHop (Board b) sl ssww rank p) || fst (validHop (Board b) sl ssee rank p) then True else False
  where 
        ssww = (pred $ pred sl_r, sl_c -2)
        ssee = (succ $ succ sl_r, sl_c -2)
        nnww = (pred $ pred sl_r, sl_c +2)
        nnee = (succ $ succ sl_r, sl_c +2)



adjacent :: Board -> Loc -> Loc -> Rank -> Player -> Bool 
adjacent (Board b) sl@(sl_r, sl_c) el rank p
          | el_piece /= Nothing = False 
          | rank == Queen  = if      sw == el then True
                             else if se == el then True
                             else if nw == el then True
                             else if ne == el then True
                             else False
          | p == OneB     =  if      nw == el then True
                             else if ne == el then True
                             else False
          | p == TwoW     =  if      sw == el then True
                             else if se == el then True
                             else False
          | otherwise = False
      where sw = (pred sl_r, sl_c -1)
            se = (succ sl_r, sl_c -1)
            nw = (pred sl_r, sl_c +1)
            ne = (succ sl_r, sl_c +1)
            el_piece = getPiece (Board b) el 


--ensures no hopping of same color, and returns location of a validly hopped piece
validHop :: Board -> Loc -> Loc -> Rank -> Player -> (Bool,Maybe Loc)
validHop (Board b) sl@(sl_r, sl_c) el rank p 
      | el_piece /= Nothing = (False, Nothing)
      | rank == Queen && p == OneB =  
                          if      ssww == el && (sw_p == white_piece || sw_p == white_king) then (True, Just sw)
                          else if ssee == el && (se_p == white_piece || se_p == white_king) then (True, Just se)
                          else if nnww == el && (nw_p == white_piece || nw_p == white_king) then (True, Just nw)
                          else if nnee == el && (ne_p == white_piece || ne_p == white_king) then (True, Just ne)
                          else (False, Nothing)
      | rank == Queen && p == TwoW = 
                          if      ssww == el && (sw_p == black_piece || sw_p == black_king) then (True, Just sw)
                          else if ssee == el && (se_p == black_piece || se_p == black_king) then (True, Just se)
                          else if nnww == el && (nw_p == black_piece || nw_p == black_king) then (True, Just nw)
                          else if nnee == el && (ne_p == black_piece || ne_p == black_king) then (True, Just ne)
                          else (False, Nothing)
      | p == OneB     =   if      nnww == el && (nw_p == white_piece || nw_p == white_king) then (True, Just nw)
                          else if nnee == el && (ne_p == white_piece || ne_p == white_king) then (True, Just ne)
                          else (False, Nothing)
      | p == TwoW     =   if      ssww == el && (sw_p == black_piece || sw_p == black_king) then (True, Just sw)
                          else if ssee == el && (se_p == black_piece || se_p == black_king) then (True, Just se)
                          else (False, Nothing)
      | otherwise = (False, Nothing)
  where ssww = (pred $ pred sl_r, sl_c -2)
        ssee = (succ $ succ sl_r, sl_c -2)
        nnww = (pred $ pred sl_r, sl_c +2)
        nnee = (succ $ succ sl_r, sl_c +2)
        sw   = (pred sl_r, sl_c -1) -- can eliminate these with proper scoping, future work 
        se   = (succ sl_r, sl_c -1)
        nw   = (pred sl_r, sl_c +1)
        ne   = (succ sl_r, sl_c +1)
        sw_p =  Map.lookup(sw) b 
        se_p =  Map.lookup(se) b
        nw_p =  Map.lookup(nw) b 
        ne_p =  Map.lookup(ne) b 
        el_piece = getPiece (Board b) el

-- working but without comprehensive testing
--sl = start location , el = end location 
--this function returns the location of a piece that was hopped over
validEnd :: Board -> Move -> Player -> (Bool,Maybe Loc)
validEnd (Board b) (sl, el) p = case (Map.lookup(el) b, getRank (Board b) sl ) of
                                     (Nothing,_) -> (False, Nothing)
                                     (Just (c, Nothing), Just rank) -> if c == White then (False, Nothing)
                                                                       else if adjacent (Board b) sl el rank p || fst (validHop (Board b) sl el rank p) then (True, snd (validHop (Board b) sl el rank p)) 
                                                                       else (False, Nothing)
                                     (Just (c, Just piece), _) -> (False, Nothing)
   -- where
         --hopResult = validHop b sl el rank p --hmmmm, rank is out of scope in this clause due to the case statement. Let instead? future work

validStart :: Board -> Loc -> Player -> Bool
validStart (Board b) l p = case Map.lookup(l) b of
                                Nothing               -> False
                                Just (c, Nothing)     -> False 
                                Just (c, Just (Piece player rank)) -> if c == White then False
                                                                      else if player == p then True
                                                                      else False

evalMove :: Board -> Player -> Move -> GameMode -> (Maybe Board, Maybe String)
--evalMove b p m = (Just (updateBoard m b), Nothing)
evalMove b p mv@(startLoc, endLoc) gm | not (validStart b startLoc p) = (Just b, Just "Invalid starting location. Please try again.")
                                      | not $ fst validEndResult      = (Just b, Just "Invalid ending location. Please try again.")
                                      | gm == Inverse && possibleHop b startLoc p  = (Just b, Just "Must take available hop move. Please try again.")
                                      | otherwise = case snd validEndResult of 
                                                      Nothing -> (Just (updateBoard mv b), Nothing)
                                                      Just l  -> (Just (updateBoard mv (removePiece b l)), Nothing)
  where validEndResult = validEnd b mv p                                                      


{-
Model TODO:
  - eventually add GameMode to the Board object 

  - maaaybe the tie function 

  - testing; so far very lightly tested
-}
