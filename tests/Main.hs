-- |
-- Module      : Main.hs
-- Description : Entry point for test suite.
-- Maintainer  : jimburton1@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- 
module Main
    where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck ( quickCheck, withMaxSuccess )

import Test.Chapter1
  ( prop_updateSquare
  , prop_updateBoard )
import Test.Chapter2
  ( prop_fillRack1
  , prop_fillRack2 )
import Test.Chapter3
  (prop_connects
  , prop_fMCentre
  , prop_lettersAvailable
  , prop_straight
  , prop_wordOnBoard)
import Test.Chapter4
  (prop_pass
  , prop_swap
  , prop_move)
import Test.Chapter5
  (prop_AIGame)
  
-- * Tests


main :: IO ()
main = do
  -- propAIGame plays an entire game so can take a long time
  quickCheck (withMaxSuccess 1 prop_AIGame)
  defaultMain tests

tests :: [Test]
tests = [ testProperty "updateSquare places a (pos,tile) on the board" prop_updateSquare
        , testProperty "updateBoard places a word on the board" prop_updateBoard
        , testProperty "fillRack moves tiles from the bag to the rack" prop_fillRack1
        , testProperty "fillRack does nothing if the rack is already full" prop_fillRack2
        , testProperty "words added to board connect with another" prop_connects
        , testProperty "first word touches the centre square" prop_fMCentre
        , testProperty "words can only be played if letters available" prop_lettersAvailable
        , testProperty "only words in a straight line can be played" prop_straight
        , testProperty "words must be on the board" prop_wordOnBoard
        , testProperty "player can take a move by passing" prop_pass
        , testProperty "player can take a move by swapping tiles" prop_swap
        , testProperty "player can take a move by placing a word on the board" prop_move
        ]

