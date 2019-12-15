# Dual Mode Checkers
###### CS 641 -- Functional Programming & Haskell
###### Authors: Sergey Goldobin and Justin Lad

## Summary
This project is an implementation of two "flavors" of 8 by 8 draughts, also 
known as checkers, in Haskell. The aforementioned "flavors" are two game modes: 
classic and inverse checkers.

Classic mode is the "by the book" implementation of checkers and obeys the
following set of rules:
* Each player starts off with 12 pieces (men).
* During a player's turn, a man can perform one of the following actions:
** Move one tile diagonally "forward", that is up a row for White and down a
row for Black.
** Hop over a piece of the opposing color that is located in a spot that would
be a valid move according to the above item.
*** The piece that was hopped over is removed from play.
*** After a hop is performed and there are more valid hop targets, a player may
hop agan.
** Reach the opposing edge of the board and transform into a Queen of the cores-
ponding color.
* Once a man transforms into a queen, they get an expanded set of moves:
** A queen may move both forwards and backwards, but only diagonally.
** A queen may hop pieces any distance away.
* The loser is a player that is the first to have no pieces left.

Inverse mode follows all of the same rules with the following twists:
* If a player is able to hop with a piece, they must use their turn to make the
hop.
* The first player to loose all of their pieces is the winner.
* The objective is to get rid of all your pieces before your opponent!

The project comes with an additional set of supporting features:
* Cursor controlled output -- Characters in the terminal are set in-place,
producing a slick, compact look.
* Autoplay -- The program accepts input files with moves that will play the game
automatically with a delay. Perfect to watch & spectate!
** Once all automatic moves have been executed, the player may pick up form there.
* Replays -- At any moment during a game, a player can save their progress so far
as a replay. The resulting file can be shared with friends or used with autoplay!


Finally, the project comes with a test suite that validates correctness of 
the underlying game model.

## Instructions

Basic usage of the program looks as follows:
> runhaskell Checkers {classic|inverse} [auto=file-name]

The classic/inverse flag denotes with mode to launch the game in: classic and 
inverse respectively. The optional 'auto' parameter is used to indicate that
the game should auto play a supplied file. For example, auto-playing a game sa-
ved in MyGame.txt can be done as follows:
> runhaskell Checkers classic auto=MyGame.txt

On the other hand, playing a game of inverse checkers can be done with the
following command:
> runhaskell Checkers inverse

To run the test suite, please use the following command:
> runhaskell Tests

## Project Structure

The project consists of the following modules:

###### Checkers
This module contains main, game's Read-Eval-Print loop, and user command parsing.
Additionally, it utilizes functions from Display.hs and Model.hs, effectively
bringing the whole program together. This is also the only module with impure
functions. Key functions and data structirs are:
* Command   -- Data structure for user command parsing.
* parseArgs -- A function that parses command line arguments and configures ga-
me mode and autoplay.
* main      -- Reads command line arguments and launches the game.
* parseMove -- Reads a command form a user and parses it into a move or other action.
* play      -- The heart and soul of the program. Executes the main REP loop.

###### Model
This module contains the core game logic. The main functions are newBoard, isWin, and evalMove.

* newBoard          -- creates a new checkers Board with all the pieces on it
* updateBoard       -- performs a valid Move on the Board, producing updated board
* checkPossibleHops -- checks if there's an available hop for inverse mode. 
* isWin             -- checks if a player is out of pieces or they don't have an possible move. Either are false, the player loses. 
* validHop          -- checks if a hop is valid
* validStart        -- checks if a Move has a valid starting location
* validEnd          -- checks if a Move has a valid ending location
* evalMove          -- computes a list of Moves by calling evalSingleMove. Only updates the board if all the moves are valid; otherwise, rolls back as an invalid Move. 
* evalMoveHelper    -- helper function to ensure Man hopping backwards can only happen after at least one hop
* evalSingleMove    -- checks validStart, validEnd, if a hop is available in inverse mode. If all of those pass, it returns an updated the Board with the Move completed. 

###### Display
This module contains all the functionality related to visualization and cursor 
controls. Additionally, this module defines all visual components and special 
characters.This module depends on Model.hs, and has the followong key components:
* {w,b}Man, {w,b}Queen -- Definitions of Unicode chars representing pieces. When
viewed with a sophisticated text editor they appear flipped, but they display
correctly in the terminal.
* stringify            -- Create a string representation of a given board.
* set* function family -- Set a cursor at the designated terminal location, 
clear it, and place new content.

###### Tests
This module serves to validate the correctness of the underlying game logic and 
is only dependent on Model.hs. We painstakingly enumerated all interesting cases
possible and created test scenarios to verify correct behavior. This module has
a main(), or can be run individually.

## Haskell Dependencies
The project does not require any additional libraries can can be compiled with
GHC and run as is.

## Debriefing (Work Done & Lessons Learned)

Work Done:
Building a checkers game in Haskell was a fairly straightforward process with good upfront planning.  We used Haskell's data type to logically organize the many components that make up the game a checkers. We used the model-view-control pattern to seperate the game logic into distinct components.  We were even able to achieve a sleek cursor controlled update to the terminal to avoid constantly printing out new game boards. Overall, this was fun to complete in Haskell, and may have been even more challenging in an imperative language. 


Lessons Learned: 
Developing the test suite would have generally been better to develop sooner in the process to verify Model.hs logic. Instead, it was used as more of an afterthought to ensure game properties held.  The addition of loading and storing replays was another great feature for testing, because it easily enabled storing game states with edge cases for testing. We primarily relied upon replays for testing. The Model.hs file was a tad verbose, but using pattern matching, guards, and where clauses to compute all the game logic was very simple and readable.  









