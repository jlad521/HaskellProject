{-
    A collection of tests for Checkers.hs
    @authors: Sergey Goldobin, Justin Lad
    11/15/2019
-}

module Tests where

import Test.HUnit
import Checkers
import Model
import Display
import Data.Map.Strict as Map

{-
    Test cases to cover:
    1) Move a Man forward and right
        1.1) To a valid spot.
        1.2) To an invalid spot.
            1.2.1) Occupied by another piece.
            1.2.2) Off the edge of the board.
    2) Move a Man forward and left with the same sub-cases as 1.
    3) "Eat" a piece with a Man in each diagonal direction with same sub-cases as 1.
    4) Move a Queen 1 or more tiles in each diagonal direction.
        4.1) On an unobstructed diagonal.
        4.2) On an obstructed diagonal.
    5) "Eat" with a queen in every diagonal direction 1 or more tiles away with same sub-cases as 1.
-}

-- White Man for testing.
wm :: (Color, Maybe Piece) -> (Color, Maybe Piece)
wm (c, _) = (c, Just (Piece TwoW Man))

-- White Queen for testing.
wq :: (Color, Maybe Piece) -> (Color, Maybe Piece)
wq (c, _) = (c, Just (Piece TwoW Queen))

-- Black Man for testing.
bm :: (Color, Maybe Piece) -> (Color, Maybe Piece)
bm (c, _) = (c, Just (Piece OneB Man))

-- Black Queen for testing.
bq :: (Color, Maybe Piece) -> (Color, Maybe Piece)
bq (c, _) = (c, Just (Piece OneB Queen))


manBoardW :: Board
manBoardW = let (Board m) = emptyBoard
            in (Board (Map.adjust wm ('D', 4) m))

manBoardB :: Board
manBoardB = let (Board m) = emptyBoard
            in  (Board (Map.adjust bm ('D', 4) m))

edgeBoardW :: Board
edgeBoardW = let (Board m) = emptyBoard
             in  (Board (Map.adjust wm ('C', 1) m))

edgeBoardB :: Board
edgeBoardB = let (Board m) = emptyBoard
             in  (Board (Map.adjust bm ('B', 8) m))

-- Board with 2 adjacent pieces of same color to ensure no movement to occupied tiles
occupiedBoardW :: Board
occupiedBoardW  = let (Board m) = manBoardW 
                  in  (Board (Map.adjust wm ('E',3) m)) 

occupiedBoardB :: Board
occupiedBoardB  = let (Board m) = manBoardB 
                  in  (Board (Map.adjust bm ('E',5) m)) 


-- Board with 2 adjacent pieces of different color to test hopping
eatBoardW :: Board 
eatBoardW = let (Board m) = manBoardW 
            in  (Board (Map.adjust bm ('E',3) m)) 

eatBoardB :: Board 
eatBoardB = let (Board m) = manBoardB 
            in  (Board (Map.adjust wm ('E',5) m)) 

wrongDirEatB :: Board
wrongDirEatB = let (Board m) = manBoardW
            in  (Board (Map.adjust bm ('E',5) m)) 

wrongDirEatW :: Board
wrongDirEatW = let (Board m) = manBoardB
               in  (Board (Map.adjust wm ('C',3) m)) 

-- need to double check the locations
eatBoardFailW :: Board 
eatBoardFailW = let (Board m) = manBoardW 
            in  (Board (Map.adjust wm ('E',3) m)) 

eatBoardFailB :: Board 
eatBoardFailB = let (Board m) = manBoardB 
                in  (Board (Map.adjust bm ('E',5) m)) 

inverseFailW :: Board
inverseFailW = let (Board m) = eatBoardW 
               in (Board (Map.adjust wm ('H',8) m)) 

inverseFailB :: Board
inverseFailB = let (Board m) = eatBoardB 
               in (Board (Map.adjust bm ('A',1) m)) 

-- inverseFailW :: Board
-- inverseFailW = let (Board m) = inverseFailWHelper 
--                 in 

-- inverseFailB :: Board 
-- inverseFailB = let (Board m) = manBoardB
--                 in 


-- Board with piece 1 row away from turning into queen. Should be a queen after the move. 
queenTestBoardW :: Board 
queenTestBoardW = let (Board m) = emptyBoard
                  in (Board (Map.adjust wm ('F', 2) m))

queenTestBoardB :: Board 
queenTestBoardB = let (Board m) = emptyBoard
                  in (Board (Map.adjust wm ('E', 7) m))
--occupiedBoard (Board b) = Map.adjust wm ('D',4) b) 

eatBoardQueenForwB :: Board 
eatBoardQueenForwB = let (Board m) = manBoardW 
                     in  (Board (Map.adjust bq ('E',3) m))

eatBoardQueenBackwB :: Board 
eatBoardQueenBackwB = let (Board m) = manBoardW
                      in  (Board (Map.adjust bq ('E',5) m)) 

eatBoardQueenForwW :: Board 
eatBoardQueenForwW = let (Board m) = manBoardB
                     in  (Board (Map.adjust wq ('E',5) m)) 

eatBoardQueenBackwW :: Board 
eatBoardQueenBackwW = let (Board m) = manBoardB 
                     in  (Board (Map.adjust wq ('C',3) m)) 


manMoveTests :: Test
manMoveTests =
  TestList [ 
             -- Invalid start location
             evalMove manBoardB OneB badStart Standard    ~?= invalidStartRes,
             evalMove manBoardW TwoW badStart Standard    ~?= invalidStartRes,

             -- Black man basic movements
             evalMove manBoardB OneB moveBL Standard      ~?= moveFRresB, -- Test 1: Move 1 tile forward right. EXPECT: Success
             evalMove manBoardB OneB moveBR Standard      ~?= moveFLresB, -- Test 2: Move 1 tile forward left.    EXPECT: Success
             evalMove manBoardB OneB moveFL Standard      ~?= invalidEnd, -- Test 3: Move 1 tile back right.      EXPECT: Fail: no backwards move
             evalMove manBoardB OneB moveFR Standard      ~?= invalidEnd, -- Test 4: Move 1 tile back left.     EXPECT: Fail: no backwards move
             evalMove manBoardB  OneB moveFL2 Standard    ~?= invalidEnd, -- Test 5: Move 2 tiles forward right.    EXPECT: Fail: > 1 tile
             evalMove manBoardB  OneB moveFR2 Standard    ~?= invalidEnd, -- Test 6: Move 2 tiles forward right.    EXPECT: Fail: > 1 tile
             evalMove edgeBoardB OneB moveFROOBB Standard ~?= invalidEnd, -- Test 7: Move FR out of bounds.  EXPECT: Fail: Out of bounds
             evalMove edgeBoardB OneB moveFLOOBB Standard ~?= invalidEnd,  -- Test 8: Move FL out of bounds.  EXPECT: Fail: Out of bounds

             -- White man basic movements 
             evalMove manBoardW TwoW moveFR Standard     ~?= moveFRresW, -- Test 1: Move 1 tile forward right.	EXPECT: Success
             evalMove manBoardW TwoW moveFL Standard     ~?= moveFLresW, -- Test 2: Move 1 tile forward left.		EXPECT: Success
             evalMove manBoardW TwoW moveBR Standard     ~?= invalidEnd, -- Test 3: Move 1 tile back right.  		EXPECT: Fail: no backwards move
             evalMove manBoardW TwoW moveBL Standard     ~?= invalidEnd, -- Test 4: Move 1 tile back left.  		EXPECT: Fail: no backwards move
             evalMove manBoardW TwoW moveFR2 Standard    ~?= invalidEnd, -- Test 5: Move 2 tiles forward right.    EXPECT: Fail: > 1 tile
             evalMove manBoardW TwoW moveFL2 Standard    ~?= invalidEnd, -- Test 6: Move 2 tiles forward right.    EXPECT: Fail: > 1 tile
             evalMove edgeBoardW TwoW moveFROOB Standard ~?= invalidEnd, -- Test 7: Move FR out of bounds.  EXPECT: Fail: Out of bounds
             evalMove edgeBoardW TwoW moveFLOOB Standard ~?= invalidEnd,  -- Test 8: Move FL out of bounds.  EXPECT: Fail: Out of bounds

            
             -- Move to destination occupied by Man.
             evalMove occupiedBoardB OneB moveBR Standard ~?= invalidEnd,
             evalMove occupiedBoardW TwoW moveFR2 Standard ~?= invalidEnd,

             
             --TODO: Hop over a man of opposite color and eat.
             evalMove eatBoardB OneB moveFL2B Standard ~?= eatBoardResB,
             evalMove eatBoardW TwoW moveFR2 Standard  ~?= eatBoardResW,

             --TODO: Hop over piece in wrong direciton as man (should fail)
             evalMove wrongDirEatB OneB moveBadDirB Standard ~?= invalidEnd,
             evalMove wrongDirEatW TwoW moveBadDirW Standard ~?= invalidEnd,

             --TODO: Fail to hop over a man of same color.
             evalMove eatBoardFailB OneB moveFL2B Standard ~?= invalidEnd,
             evalMove eatBoardFailW TwoW moveFR2 Standard ~?= invalidEnd,

             --TODO: test if you can mkae multiple jumps in one turn! 

             -- Check that inverse mode enforces a move
             evalMove inverseFailB OneB invFailMvB Inverse ~?= inverseFailRes,
             evalMove inverseFailW TwoW invFailMvW Inverse ~?= inverseFailRes

             --TODO: Become a queen.
             --evalMove queenTestBoardB OneB moveFRQ Standard ~?= moveQueenResB, -- test if it produces queen for white player
             --evalMove queenTestBoardW TwoW moveFRQ Standard ~?= moveQueenResW -- test if it produces queen for white player
             
           ]
    where badStart = (('F',3),('D',4))
          moveFR = (('D', 4), ('E', 3))
          moveFL = (('D', 4), ('C', 3))
          moveBR = (('D', 4), ('E', 5))
          moveBL = (('D', 4), ('C', 5))

          moveFRQ = (('F',3), ('E',1))
          moveBLQ = (('E',7), ('F',8))

          moveFR2 = (('D', 4), ('F', 2))
          moveFL2 = (('D', 4), ('B', 2))
          moveFL2B = (('D', 4), ('F', 6))

          moveFROOB = (('C', 1), ('D', 0))
          moveFLOOB = (('C', 1), ('B', 0))

          moveFROOBB = (('B', 8), ('A', 9))
          moveFLOOBB = (('B', 8), ('C', 9))

          moveBadDirB = (('E',5),('C',3))
          moveBadDirW = (('C',3),('E',5))
         
          invFailMvB = (('A',1),('B',2))
          invFailMvW = (('H',8),('G',7))




queenMoveTests :: Test
queenMoveTests = 
  TestList [

          -- basic queen movements 
          --necessary??
            
             -- Hop over over man in normal direction 
             evalMove eatBoardQueenForwB OneB queenMvSE Standard   ~?= queenEatForwResB,
             evalMove eatBoardQueenForwW TwoW queenMvNE Standard  ~?= queenEatForwResW,

             -- Hop over man in opposite direction 
             evalMove eatBoardQueenBackwB OneB queenMvNE Standard ~?= queenEatBkwrdResB,
             evalMove eatBoardQueenBackwW TwoW queenMvW Standard  ~?= queenEatBkwrdResW


            
           ]
  where 
       queenMvSE = (('E',3),('C',5))
       queenMvNE = (('E',5),('C',3))
       queenMvW  = (('C',3),('E',5))

{- TEST RESULTS -}



invalidEnd :: (Maybe Board, Maybe String)
invalidEnd =  (Nothing, Just "Invalid ending location. Please try again.")

inverseFailRes :: (Maybe Board, Maybe String)
inverseFailRes = (Nothing, Just "Must take available hop move. Please try again.")

invalidStartRes :: (Maybe Board, Maybe String)
invalidStartRes =  (Nothing, Just "Invalid starting location. Please try again.")           

moveFRresW :: (Maybe Board, Maybe String)
moveFRresW = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust wm ('E', 3) m)), Nothing)

moveFLresW :: (Maybe Board, Maybe String)
moveFLresW = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust wm ('C', 3) m)), Nothing)

moveFRresB :: (Maybe Board, Maybe String)
moveFRresB = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust bm ('C', 5) m)), Nothing)

moveFLresB :: (Maybe Board, Maybe String)
moveFLresB = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust bm ('E', 5) m)), Nothing)

eatBoardResW :: (Maybe Board, Maybe String)
eatBoardResW = let (Board m) = emptyBoard
               in (Just (Board (Map.adjust wm ('F', 2) m)), Nothing) 

eatBoardResB :: (Maybe Board, Maybe String)
eatBoardResB = let (Board m) = emptyBoard
               in (Just (Board (Map.adjust bm ('F', 6) m)), Nothing) 

moveQueenResW :: (Maybe Board, Maybe String)
moveQueenResW = let (Board m) = emptyBoard
               in (Just (Board (Map.adjust wq ('E', 1) m)), Nothing)

moveQueenResB :: (Maybe Board, Maybe String)
moveQueenResB = let (Board m) = emptyBoard
                in (Just (Board (Map.adjust bq ('F', 8) m)), Nothing)


queenEatForwResB :: (Maybe Board, Maybe String)
queenEatForwResB = let (Board m) = emptyBoard
                   in (Just (Board (Map.adjust bq ('C', 5) m)), Nothing)

queenEatForwResW :: (Maybe Board, Maybe String)
queenEatForwResW = let (Board m) = emptyBoard
                   in (Just (Board (Map.adjust wq ('C', 3) m)), Nothing)

queenEatBkwrdResB :: (Maybe Board, Maybe String)
queenEatBkwrdResB = let (Board m) = emptyBoard
                   in (Just (Board (Map.adjust bq ('C', 3) m)), Nothing)

queenEatBkwrdResW :: (Maybe Board, Maybe String)
queenEatBkwrdResW = let (Board m) = emptyBoard
                   in (Just (Board (Map.adjust wq ('E', 5) m)), Nothing)
{- ##################### -}

-- Carry out all the tests.
main :: IO ()
main = do runTestTT manMoveTests
          runTestTT queenMoveTests
          --runTestTT winTests
          return ()


{-
unused 

moveFR2res :: (Maybe Board, Maybe String)
moveFR2res = let (Board m) = emptyBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFL2res :: (Maybe Board, Maybe String)
moveFL2res = let (Board m) = emptyBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFROOBres :: (Maybe Board, Maybe String)
moveFROOBres = let (Board m) = edgeBoardW
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFLOOBres :: (Maybe Board, Maybe String)
moveFLOOBres = let (Board m) = edgeBoardW
               in (Nothing, Just "Invalid ending location. Please try again.")


invalidStartRes :: (Maybe Board, Maybe String)
invalidStartRes = let (Board m) = manBoardW
                   in (Nothing, Just "Invalid starting location. Please try again.")           

invalidStartResW :: (Maybe Board, Maybe String)
invalidStartResW = let (Board m) = manBoardW
                   in (Nothing, Just "Invalid starting location. Please try again.")           

invalidStartResB :: (Maybe Board, Maybe String)
invalidStartResB = let (Board m) = manBoardB
                   in (Nothing, Just "Invalid starting location. Please try again.")           

invalidEndEmpty :: (Maybe Board, Maybe String)
invalidEndEmpty = let (Board m) = emptyBoard
                in (Nothing, Just "Invalid ending location. Please try again.")

moveOccupiedW :: (Maybe Board, Maybe String)
moveOccupiedW = let (Board m) = occupiedBoardW
                in (Nothing, Just "Invalid ending location. Please try again.")

moveOccupiedB :: (Maybe Board, Maybe String)
moveOccupiedB = let (Board m) = occupiedBoardB
                in (Nothing, Just "Invalid ending location. Please try again.")

invalidEndManBoardW :: (Maybe Board, Maybe String)
invalidEndManBoardW = let (Board m) = manBoardW
                      in (Nothing, Just "Invalid ending location. Please try again.")

invalidEndManBoardB :: (Maybe Board, Maybe String)
invalidEndManBoardB = let (Board m) = manBoardB
                      in (Nothing, Just "Invalid ending location. Please try again.")




-}








