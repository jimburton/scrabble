module Main
    where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.Chapter1
  ( prop_updateSquare
  , prop_updateBoard )
import Test.Chapter2
  ( prop_fillRack1
  , prop_fillRack2 )
import Test.Chapter3
  ( prop_straight
  , prop_wordOnBoard
  , prop_fMCentre
  , prop_connects
  , prop_lettersAvailable)
  
-- ============= Tests ================ --


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "updateSquare places a (pos,tile) on the board" prop_updateSquare
        , testProperty "updateBoard places a word on the board" prop_updateBoard
        , testProperty "fillRack moves tiles from the bag to the rack" prop_fillRack1
        , testProperty "fillRack does nothing if the rack is already full" prop_fillRack2
        , testProperty "Words played must be in a straight line" prop_straight
        , testProperty "Words played must be on the board" prop_wordOnBoard
        , testProperty "The first move must touch the centre square" prop_fMCentre
        , testProperty "Words played must connect to others" prop_connects
        , testProperty "Words played must come from available tiles" prop_lettersAvailable
        ]

