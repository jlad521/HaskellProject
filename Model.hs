{-
  CS 641: Checkers Project
  Checkers Logic functions

  @authors: Sergey Goldobin, Justin Lad
  12/13/2019
-}

module Model where

import Data.Char -- import ord fn 
import qualified Data.Map.Strict as Map

{-
	Data structure representing the game board.
	Contains a mapping of Locations to tuples of tile Colors and Pieces.
-}
data Board  = Board (Map.Map (Char, Int) (Color, Maybe Piece))
    deriving (Show, Eq)

{-
	Color of a game tile -- Black or White.
	Used for convenient move rejection.
-}
data Color  = Black | White 
    deriving (Show, Eq)

{-
	Data structure representing a piece on the game board.
	Contains the player that owns the piece and the Rank.
-}
data Piece  = Piece Player Rank
    deriving (Show, Eq)

{-
	The Rank of a checkers piece -- Man or Queen.
-}

data Rank   = Man | Queen
    deriving (Show, Eq)

{-
	Enumeration of players.
-}
data Player = OneB | TwoW
    deriving (Eq)

{-
	Custom implementation of Show for Players
-}
instance Show Player where
  show OneB = "Black"
  show TwoW = "White"

{-
	A location is a tuple of Char column and Int row.
-}
type Loc = (Char, Int)

{-
	A move is a tuple of two Locations
-}
type Move = (Loc, Loc)

{-
	Enumeration for game modes.
-}
data GameMode = Standard | Inverse
    deriving (Show, Eq)


{-
  Produces a new board with all the pieces on it
-}
newBoard :: Board
newBoard = addPieces (Board (Map.fromList([ ((c, i), (White, Nothing)) | c <- ['A' .. 'H'], i <- [1..8]])))

{-
  Board without pieces for testing.
-}
emptyBoard :: Board
emptyBoard = Board (Map.mapWithKey toss nm)
  where toss :: Loc -> (Color, Maybe Piece) -> (Color, Maybe Piece)
        toss key val = case val of
                         (c, Just _) -> (c, Nothing)
                         v           -> v 
        (Board nm) = newBoard

-- global definitions of pieces to reduce repitition of definitions
black_piece = Just (Black, Just(Piece OneB Man))
white_piece = Just (Black, Just(Piece TwoW Man))
black_king  = Just (Black, Just(Piece OneB Queen))
white_king  = Just (Black, Just(Piece TwoW Queen))

-- used as the Board for starting a new game 
actual_board :: Board 
actual_board = newBoard

{-
  Puts all the starting pieces on an empty board (used in newBoard function)
-}
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

{-
  updates the board with the verified valid move, producing the updated board
-}
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

{-
  get function that returns a Piece at the specified Location 
-}
getPiece :: Board -> Loc -> Maybe Piece
getPiece (Board b) l = case Map.lookup l b of
                            Nothing -> Nothing
                            Just (c, Nothing) -> Nothing
                            Just (c, Just p) -> Just p

{-
  get function that returns a Tile at the specified Location 
-}
getTile :: Board -> Loc -> (Color, Maybe Piece)
getTile (Board b) l = case Map.lookup l b of
                         Nothing -> (White, Nothing)
                         Just x  -> x

{-
  get function that returns the Rank of a piece at the specified Location 
-}
getRank :: Board -> Loc -> Maybe Rank
getRank (Board b) l = case Map.lookup l b of
                            Nothing -> Nothing
                            Just (c, Nothing) -> Nothing
                            Just (c, Just (Piece player rank)) -> Just rank
{-
  Removes a piece at a specified location, producing an updated board
-}
removePiece :: Board -> Loc -> Board
removePiece (Board b) (c,i) = (Board (Map.insert (c,i) (Black, Nothing) b))


{-
  determines if the input player has won the game or not
  handles inverse game mode by default, no need for inverting logic when called in inverse mode
-}
isWin :: Board -> Player -> GameMode -> Bool
isWin (Board b) p m = noPiecesLeft (Board b) p m || noMovesLeft (Board b) p


{-
  we decided not to implement ties because they are so rare and difficult to compute
  see http://www.darkfish.com/checkers/rules.html
-}
isTie :: Board -> Bool
isTie = undefined

{-
  checks if a player is out of pieces, for use in the isWin function 
-}
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

{-
  checks if there are any moves left for isWin
-}
noMovesLeft :: Board -> Player -> Bool
noMovesLeft (Board b) p = aux (Map.toList b) p 
  where aux [] _      = True 
        aux (t:ts) p' = ( possibleHop (Board b) l p || possibleAdjacent (Board b) l p ) && aux ts p'
            where l  = fst t 
{-
  checks if there's a possible hop to enforce hop for inverse game mode & isWin function
-}
checkPossibleHops :: Board -> Player -> Bool
checkPossibleHops (Board b) pl = aux (Map.toList b) pl
  where aux []     _ = False 
        aux (t:ts) p = possibleHop (Board b) l p || aux ts p 
            where l  = fst t 

{-
  checks if there's a possible adjacent move for isWin function. 
  If no moves are available for the player, they will lose the game 
-}
possibleAdjacent :: Board -> Loc -> Player -> Bool
possibleAdjacent (Board b) sl@(sl_r, sl_c) p=
        case getPiece (Board b) sl of 
             Nothing -> False
             Just (Piece player rank) ->
               case (player, rank) of 
                  (OneB, Man) -> adjacent (Board b) sl nw Man p   || adjacent (Board b) sl ne Man p
                  (TwoW, Man) -> adjacent (Board b) sl sw Man p   || adjacent (Board b) sl se Man p
                  (_, Queen)  -> adjacent (Board b) sl nw Queen p || adjacent (Board b) sl ne Queen p 
                                 || adjacent (Board b) sl sw Queen p || adjacent (Board b) sl se Queen p
  where 
        sw = (pred sl_r, sl_c -1)
        se = (succ sl_r, sl_c -1)
        nw = (pred sl_r, sl_c +1)
        ne = (succ sl_r, sl_c +1)

{-
  checks if there's a possible hop, for inverse mode
-}
possibleHop :: Board -> Loc -> Player -> Bool
possibleHop (Board b) sl@(sl_r, sl_c) p = 
        case getPiece (Board b) sl of
          Nothing -> False
          Just (Piece player rank) -> 
             case (player, rank) of 
                (OneB, Man)   -> fst (validHop (Board b) sl nnww rank p False) || fst (validHop (Board b) sl nnee rank p False)
                (TwoW, Man)   -> fst (validHop (Board b) sl ssww rank p False) || fst (validHop (Board b) sl ssee rank p False)
                (_, Queen)    -> fst (validHop (Board b) sl nnww rank p False) || fst (validHop (Board b) sl nnee rank p False)
                                 || fst (validHop (Board b) sl ssww rank p False) || fst (validHop (Board b) sl ssee rank p False)
  where 
        ssww = (pred $ pred sl_r, sl_c -2)
        ssee = (succ $ succ sl_r, sl_c -2)
        nnww = (pred $ pred sl_r, sl_c +2)
        nnee = (succ $ succ sl_r, sl_c +2)


{-
  checks if adjacent square is a valid move, for use in validHop
-}
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

{-
 function that determines if the location is in bounds, to prevent out of bound hops
-}
inBounds :: Loc -> Bool
inBounds el@(row, col) = ord row >= ord 'A' && ord row <= ord 'H' && col >= 1 && col <= 8

{-
  ensures no hopping of same color, and hop over opposite color
  returns location of a validly hopped piece
-}
validHop :: Board -> Loc -> Loc -> Rank -> Player -> Bool -> (Bool,Maybe Loc)
validHop (Board b) sl@(sl_r, sl_c) el rank p flag
      | el_piece /= Nothing = (False, Nothing)
      | (rank == Queen && p == OneB)  || (p == OneB && flag) =  
                          if      ssww == el && (sw_p == white_piece || sw_p == white_king) && inBounds el then (True, Just sw)
                          else if ssee == el && (se_p == white_piece || se_p == white_king) && inBounds el then (True, Just se)
                          else if nnww == el && (nw_p == white_piece || nw_p == white_king) && inBounds el then (True, Just nw)
                          else if nnee == el && (ne_p == white_piece || ne_p == white_king) && inBounds el then (True, Just ne)
                          else (False, Nothing)
      | (rank == Queen && p == TwoW) || (p == TwoW && flag) =
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

{-
  takes a move and player, and returns the following: 
  first bool indicates if it's a valid move. 
  second bool indicates if a piece was hopped (needed for inverse game mode)
  Maybe Location: returns the location of the ending location if valid 
  sl = start location , el = end location 
-}
validEnd :: Board -> Move -> Player -> Bool -> (Bool, Bool, Maybe Loc)
validEnd brd@(Board b) (sl, el) p flag = 
    case r of
      Nothing   -> (False, False, Nothing)
      Just rank -> case (Map.lookup(el) b) of
                      Just (Black, Nothing) -> if adjacent brd sl el rank p 
                                               then (True, False, hopLoc)
                                               else if hopValid then (True, True, hopLoc)
                                               --then (True, hopLoc) 
                                               else (False, False, Nothing)
                      _                     -> (False, False, Nothing) -- Any destination that is not a black empty tile is invalid. 
          where (hopValid, hopLoc) = validHop brd sl el rank p flag
  where r = case Map.lookup sl b of
                  Just (_, Just (Piece _ rnk)) -> Just rnk
                  _                            -> Nothing
        

{-
  validates start location of a move. True if it's a valid hop 
-}
validStart :: Board -> Loc -> Player -> Bool
validStart (Board b) l p = case Map.lookup(l) b of
                             Just (Black, Just (Piece player _)) -> player == p -- Only valid start is an existing piece belonging to player.
                             _                                   -> False
{-
  evaluates a list of Moves. If any of the moves in the list are invalid, sends an invalid error and starts player's move again
-}
evalMove :: Board -> Player -> [Move] -> GameMode -> (Maybe Board, Maybe String)
evalMove b p [] gm     = evalMoveHelper b p [] gm False False
evalMove b p [mv] gm   = evalSingleMove b p mv gm False False
evalMove b p (m:mv) gm = case evalSingleMove b p m gm False True of
                               (Just b, Nothing) -> evalMoveHelper b p mv gm True True
                               (Nothing, Just e) -> (Nothing, Just e)

evalMoveHelper :: Board -> Player -> [Move] -> GameMode -> Bool -> Bool -> (Maybe Board, Maybe String)
evalMoveHelper b _ [] _ _ _  = (Just b, Just "error on evalMove list")
evalMoveHelper b p [mv] gm flag multi = evalSingleMove b p mv gm flag False
evalMoveHelper b p (m:mv) gm flag multi = case evalSingleMove b p m gm flag multi of
                                            (Just b, Nothing) -> evalMoveHelper b p mv gm flag multi
                                            (Nothing, Just e) -> (Nothing, Just e)

{-                               
  core logic function. Verifies that starting location is valid, ending location is valid, and if a hop is available for inverse mode
  If move is valid, it updates the game board. It returns either two responses:
  (updated Board, nothing) if valid; (nothing, "errorMessage") otherwise
-}
evalSingleMove :: Board -> Player -> Move -> GameMode -> Bool -> Bool -> (Maybe Board, Maybe String)
evalSingleMove b p mv@(startLoc, endLoc) gm flag multi 
    | not (validStart b startLoc p) = (Nothing, Just "Invalid starting location. Please try again.")
    | not $ valid_move              = (Nothing, Just "Invalid ending location. Please try again.")
    | multi && (not isHop)          = (Nothing, Just "Cannot perform multiple moves without hops.")
    | gm == Inverse && (checkPossibleHops b p && not isHop)  = (Nothing, Just "Must take available hop move. Please try again.")
    | otherwise = case end_loc of 
                    Nothing -> (Just (updateBoard mv b), Nothing)
                    Just l  -> (Just (updateBoard mv (removePiece b l)), Nothing)
  where (valid_move, isHop, end_loc) = validEnd b mv p flag 

