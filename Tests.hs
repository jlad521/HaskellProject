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

-- Black Man for testing.
bm :: (Color, Maybe Piece) -> (Color, Maybe Piece)
bm (c, _) = (c, Just (Piece OneB Man))

manBoard :: Board
manBoard = let (Board m) = emptyBoard
            in (Board (Map.adjust wm ('D', 4) m))

edgeManBoard :: Board
edgeManBoard = let (Board m) = emptyBoard
                in (Board (Map.adjust wm ('C', 1) m))

manMoveTests :: Test
manMoveTests =
  TestList [ 
             -- Generic
             -- TODO: Invalid start location
 
             -- White man
             evalMove manBoard TwoW moveFR Standard ~?= moveFRres, -- Test 1: Move 1 tile forward right.	EXPECT: Success
             evalMove manBoard TwoW moveFL Standard ~?= moveFLres, -- Test 2: Move 1 tile forward left.		EXPECT: Success
             evalMove manBoard TwoW moveBR Standard ~?= moveBRres, -- Test 3: Move 1 tile back right.  		EXPECT: Fail: no backwards move
             evalMove manBoard TwoW moveBL Standard ~?= moveBLres, -- Test 4: Move 1 tile back left.  		EXPECT: Fail: no backwards move

             evalMove manBoard TwoW moveFR2 Standard ~?= moveFR2res, -- Test 5: Move 2 tiles forward right.		EXPECT: Fail: > 1 tile
             evalMove manBoard TwoW moveFL2 Standard ~?= moveFL2res, -- Test 6: Move 2 tiles forward right.		EXPECT: Fail: > 1 tile
             evalMove edgeManBoard TwoW moveFROOB Standard ~?= moveFROOBres, -- Test 7: Move FR out of bounds.  EXPECT: Fail: Out of bounds
             evalMove edgeManBoard TwoW moveFLOOB Standard ~?= moveFLOOBres  -- Test 8: Move FL out of bounds.  EXPECT: Fail: Out of bounds

             --TODO: Move to destination occupied by Man.
             --TODO: Hop over a man of opposite color and eat.
             --TODO: Fail to hop over a man of same color.
             --TODO: Become a queen.

             -- Black man
             --TODO: All of the same tests, but for a black piece (i.e. in opposite direction)
           ]
    where moveFR = (('D', 4), ('E', 3))
          moveFL = (('D', 4), ('C', 3))
          moveBR = (('D', 4), ('E', 5))
          moveBL = (('D', 4), ('C', 5))

          moveFR2 = (('D', 4), ('F', 2))
          moveFL2 = (('D', 4), ('B', 2))
          moveFROOB = (('C', 1), ('D', 0))
          moveFLOOB = (('C', 1), ('B', 0))

queenMoveTests :: Test
queenMoveTests = 
  TestList [

           ]

{- TEST RESULTS -}
moveFRres :: (Maybe Board, Maybe String)
moveFRres = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust wm ('E', 3) m)), Nothing)

moveFLres :: (Maybe Board, Maybe String)
moveFLres = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust wm ('C', 3) m)), Nothing)

moveBRres :: (Maybe Board, Maybe String)
moveBRres = let (Board m) = emptyBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveBLres :: (Maybe Board, Maybe String)
moveBLres = let (Board m) = emptyBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFR2res :: (Maybe Board, Maybe String)
moveFR2res = let (Board m) = emptyBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFL2res :: (Maybe Board, Maybe String)
moveFL2res = let (Board m) = emptyBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFROOBres :: (Maybe Board, Maybe String)
moveFROOBres = let (Board m) = edgeManBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

moveFLOOBres :: (Maybe Board, Maybe String)
moveFLOOBres = let (Board m) = edgeManBoard
             in (Nothing, Just "Invalid ending location. Please try again.")

{- ##################### -}

-- Carry out all the tests.
main :: IO ()
main = do runTestTT manMoveTests
          runTestTT queenMoveTests
          return ()








