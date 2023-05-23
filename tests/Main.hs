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

import Test.Chapter1
  ( prop_updateSquare
  , prop_updateBoard )
import Test.Chapter2
  ( prop_fillRack1
  , prop_fillRack2 )
  
-- * Tests


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "updateSquare places a (pos,tile) on the board" prop_updateSquare
        , testProperty "updateBoard places a word on the board" prop_updateBoard
        , testProperty "fillRack moves tiles from the bag to the rack" prop_fillRack1
        , testProperty "fillRack does nothing if the rack is already full" prop_fillRack2
        ]

