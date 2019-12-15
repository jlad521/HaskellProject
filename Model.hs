{-
  CS 641: Checkers Project
  Checkers Logic functions

  @authors: Sergey Goldobin, Justin Lad
  12/13/2019
-}

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
    deriving (Eq)

instance Show Player where
  show OneB = "Black"
  show TwoW = "White"

type Loc = (Char, Int)

type Move = (Loc, Loc)

data GameMode = Standard | Inverse
    deriving (Show, Eq)


{-
Test function area

--b = new
p1 = (Black, Just(Piece OneB Man))
p2 = (Black, Just(Piece TwoW Man))

test_valid = updateBoard (('B',6),('B',4)) actual_board

valid_move = validEnd test_valid (('A',3),('C',5)) OneB
invalid_move = validEnd test_valid (('A',3),('B',4)) OneB

test_move = (('H',6),('G',5))
test_start = ('H',6)

possibleHopTest = possibleHop test_valid ('A',3) OneB


end
-}



newBoard :: Board
newBoard = addPieces (Board (Map.fromList([ ((c, i), (White, Nothing)) | c <- ['A' .. 'H'], i <- [1..8]])))

-- Board without pieces for testing.
emptyBoard :: Board
emptyBoard = Board (Map.mapWithKey toss nm)
  where toss :: Loc -> (Color, Maybe Piece) -> (Color, Maybe Piece)
        toss key val = case val of
                         (c, Just _) -> (c, Nothing)
                         v           -> v 
        (Board nm) = newBoard

--define these as global, for use in various functions
black_piece = Just (Black, Just(Piece OneB Man))
white_piece = Just (Black, Just(Piece TwoW Man))
black_king  = Just (Black, Just(Piece OneB Queen))
white_king  = Just (Black, Just(Piece TwoW Queen))
actual_board = newBoard

-- adds pieces of appropriate color to the previously empty Board
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

-- updates the board with the verified valid move, producing the updated board
updateBoard :: Move -> Board -> Board 
updateBoard (ol, dl) (Board brd) = nb
    where tile :: Maybe (Color, Maybe Piece)
          tile = Map.lookup ol brd -- look up tile of origin
          nb   =  case tile of
                    Just (c, Just p) -> if snd dl == 1 then Board (Map.adjust (\ tup -> (fst tup,  Just(Piece TwoW Queen))) dl am) 
                                        else if snd dl == 8 then Board (Map.adjust (\ tup -> (fst tup, Just(Piece OneB Queen))) dl am)
                                        else Board (Map.adjust (\ tup -> (fst tup, Just p)) dl am)       -- Move piece from OL to DL
                    _                -> Board brd -- Something went wrong, cancel update.
          am = Map.adjust (\ tup -> (fst tup, Nothing)) ol brd -- update OL to nothing

getPiece :: Board -> Loc -> Maybe Piece
getPiece (Board b) l = case Map.lookup l b of
                            Nothing -> Nothing
                            Just (c, Nothing) -> Nothing
                            Just (c, Just p) -> Just p

getTile :: Board -> Loc -> (Color, Maybe Piece)
getTile (Board b) l = case Map.lookup l b of
                         Nothing -> (White, Nothing)
                         Just x  -> x

getRank :: Board -> Loc -> Maybe Rank
getRank (Board b) l = case Map.lookup l b of
                            Nothing -> Nothing
                            Just (c, Nothing) -> Nothing
                            Just (c, Just (Piece player rank)) -> Just rank

removePiece :: Board -> Loc -> Board
removePiece (Board b) (c,i) = (Board (Map.insert (c,i) (Black, Nothing) b))


-- handles inverse game mode, no need for inverting logic when called in inverse mode
isWin :: Board -> Player -> GameMode -> Bool
isWin (Board b) p m = noPiecesLeft (Board b) p m || noMovesLeft (Board b) p


-- we decided not to implement ties because they are so rare and difficult to compute
-- see http://www.darkfish.com/checkers/rules.html
isTie :: Board -> Bool
isTie = undefined

-- checks if a player is out of pieces to check 
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


-- checks if there are any moves left for isWin
noMovesLeft :: Board -> Player -> Bool
noMovesLeft (Board b) p = aux (Map.toList b) p 
  where aux [] _      = True 
        aux (t:ts) p' = ( possibleHop (Board b) l p || possibleAdjacent (Board b) l p ) && aux ts p'
            where l  = fst t 

-- checks if there's a possible hop for inverse 
checkPossibleHops :: Board -> Player -> Bool
checkPossibleHops (Board b) pl = aux (Map.toList b) pl
  where aux []     _ = False 
        aux (t:ts) p = possibleHop (Board b) l p || aux ts p 
            where l  = fst t 

-- checks if there's a possible adjacent move for isWin. 
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


-- checks if there's a possible hop, for inverse mode
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


-- checks if adjacent square is a valid move
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

inBounds :: Loc -> Bool
inBounds el@(row, col) = ord row >= ord 'A' && ord row <= ord 'H' && col >= 1 && col <= 8

--ensures no hopping of same color, and returns location of a validly hopped piece
validHop :: Board -> Loc -> Loc -> Rank -> Player -> (Bool,Maybe Loc)
validHop (Board b) sl@(sl_r, sl_c) el rank p 
      | el_piece /= Nothing = (False, Nothing)
      | rank == Queen && p == OneB =  
                          if      ssww == el && (sw_p == white_piece || sw_p == white_king) && inBounds el then (True, Just sw)
                          else if ssee == el && (se_p == white_piece || se_p == white_king) && inBounds el then (True, Just se)
                          else if nnww == el && (nw_p == white_piece || nw_p == white_king) && inBounds el then (True, Just nw)
                          else if nnee == el && (ne_p == white_piece || ne_p == white_king) && inBounds el then (True, Just ne)
                          else (False, Nothing)
      | rank == Queen && p == TwoW = 
                          if      ssww == el && (sw_p == black_piece || sw_p == black_king) && inBounds el then (True, Just sw)
                          else if ssee == el && (se_p == black_piece || se_p == black_king) && inBounds el then (True, Just se)
                          else if nnww == el && (nw_p == black_piece || nw_p == black_king) && inBounds el then (True, Just nw)
                          else if nnee == el && (ne_p == black_piece || ne_p == black_king) && inBounds el then (True, Just ne)
                          else (False, Nothing)
      | p == OneB     =   if      nnww == el && (nw_p == white_piece || nw_p == white_king) && inBounds el then (True, Just nw)
                          else if nnee == el && (ne_p == white_piece || ne_p == white_king) && inBounds el then (True, Just ne)
                          else (False, Nothing)
      | p == TwoW     =   if      ssww == el && (sw_p == black_piece || sw_p == black_king) && inBounds el then (True, Just sw)
                          else if ssee == el && (se_p == black_piece || se_p == black_king) && inBounds el then (True, Just se)
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

-- sl = start location , el = end location 
-- first bool indicates if it's a valid move. 
-- second bool indicates if a piece was hopped (needed for inverse game mode)
-- Maybe Location: returns the location of the ending location if valid 
validEnd :: Board -> Move -> Player -> (Bool, Bool, Maybe Loc)
validEnd brd@(Board b) (sl, el) p = 
    case r of
      Nothing   -> (False, False, Nothing)
      Just rank -> case (Map.lookup(el) b) of
                      Just (Black, Nothing) -> if adjacent brd sl el rank p 
                                               then (True, False, hopLoc)
                                               else if hopValid then (True, True, hopLoc)
                                               --then (True, hopLoc) 
                                               else (False, False, Nothing)
                      _                     -> (False, False, Nothing) -- Any destination that is not a black empty tile is invalid. 
          where (hopValid, hopLoc) = validHop brd sl el rank p
  where r = case Map.lookup sl b of
                  Just (_, Just (Piece _ rnk)) -> Just rnk
                  _                            -> Nothing
        

-- validates start location of a move.
validStart :: Board -> Loc -> Player -> Bool
validStart (Board b) l p = case Map.lookup(l) b of
                             Just (Black, Just (Piece player _)) -> player == p -- Only valid start is an existing piece belonging to player.
                             _                                   -> False
evalMove :: Board -> Player -> [Move] -> GameMode -> (Maybe Board, Maybe String)
evalMove b _ [] _    = (Just b, Just "error on evalMove list")
evalMove b p [mv] gm = evalSingleMove b p mv gm 
evalMove b p (m:mv) gm = case evalSingleMove b p m gm of
                               (Just b, Nothing) -> evalMove b p mv gm 
                               (Nothing, Just e) -> (Nothing, Just e)
-- core logic function. Verifies that a move is valid, and produces 
-- (updated Board, nothing) if valid; (nothing, "errorMessage") otherwise
evalSingleMove :: Board -> Player -> Move -> GameMode -> (Maybe Board, Maybe String)
evalSingleMove b p mv@(startLoc, endLoc) gm
    | not (validStart b startLoc p) = (Nothing, Just "Invalid starting location. Please try again.")
    | not $ valid_move     = (Nothing, Just "Invalid ending location. Please try again.")
    | gm == Inverse && (checkPossibleHops b p && not isHop)  = (Nothing, Just "Must take available hop move. Please try again.")
    | otherwise = case end_loc of 
                    Nothing -> (Just (updateBoard mv b), Nothing)
                    Just l  -> (Just (updateBoard mv (removePiece b l)), Nothing)
  where (valid_move, isHop, end_loc) = validEnd b mv p   

