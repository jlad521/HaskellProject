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
            in (Board (Map.adjust wm ('E', 4) m))

manMoveTests :: Test
manMoveTests =
  TestList [ evalMove manBoard TwoW moveFR Standard ~?= moveFRres -- Test: Move White Man 1 tile top right. EXPECT: Success

           ]
    where moveFR = (('E', 4), ('F', 3))

moveFRres :: (Maybe Board, Maybe String)
moveFRres = let (Board m) = emptyBoard
             in (Just (Board (Map.adjust wm ('F', 3) m)), Just "MSG")

-- Carry out all the tests.
main :: IO ()
main = do runTestTT manMoveTests
          return ()








