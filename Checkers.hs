{-
	CS 641: Checkers Project
	Main driver for the program.

	@authors: Sergey Goldobin, Justin Lad
	10/31/2019
-}

import System.Environment
import System.IO

import Display.hs
import Model.hs

data Mode = Classic | Inverse
    deriving (Show, Read)

-- Parse command line arguments and dispatch the appropriate game loop.
main :: IO ()
main = undefined


play :: Mode -> IO ()
play = undefined
