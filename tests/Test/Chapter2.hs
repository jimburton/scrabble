module Test.Chapter2
  where

import Test.QuickCheck.Gen
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick)
import Data.Bifunctor (first)
import Test.Gen
import Data.Array
import Lens.Simple ((^.))
import System.Random (getStdGen)
import Control.Monad.IO.Class (liftIO)

import Scrabble.Types (Letter(A))
import Scrabble.Bag
  ( newBag
  , fillRack )
  
import Scrabble.Dict (englishDictionary)
import Scrabble.Pretty() -- for the Show instance of Game

-- ============= Tests for Chapter 2 =========== --

-- | Test that using @fillRack@ takes the tiles
--   from the bag and puts them in the rack.
prop_fillRack1 :: Property 
prop_fillRack1 = monadicIO $ do
  g <- liftIO $ getStdGen
  let b = newBag
      (r',b',g') = fillRack [] b g
  assert $ (length r' == 7) && (length b' == length b - 7)

-- | Test that using @fillRack@ to fill a full rack
--   does nothing.
prop_fillRack2 :: Property 
prop_fillRack2 = monadicIO $ do
  g <- liftIO $ getStdGen
  let b = newBag
      r = replicate 7 A
      (r',b',_) = fillRack  r b g
  assert $ (r' == r) && (b' == b)
