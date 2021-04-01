module Main
    where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.Chapter1
  ( prop_updateSquare
  , prop_updateBoard )
  
-- ============= Tests ================ --


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "updateSquare places a (pos,tile) on the board" prop_updateSquare
        , testProperty "updateBoard places a word on the board" prop_updateBoard
        ]

