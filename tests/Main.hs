module Main
    where

-- import Debug.Trace
import Data.Array
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Scrabble.Board
  ( newBoard
  , updateBoard
  , updateSquare
  , wordOnRow
  , wordOnCol
  , incCol
  , incRow )
import Scrabble.Dict (scoreLetter)
import Scrabble.Types

import Test.Chapter1
  ( prop_newBoardSize
  , prop_updateSquare
  , prop_updateBoard )
  
-- ============= Tests ================ --


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "newBoard is the right size" prop_newBoardSize
        , testProperty "updateSquare places a (pos,tile) on the board" prop_updateSquare
        , testProperty "updateBoard places a word on the board" prop_updateBoard
        ]

