module Test.Chapter1
  where

import Test.QuickCheck.Gen
import Data.Bifunctor (first)
import Test.Gen
import Data.Array

import Scrabble.Types
import Scrabble.Board

-- ============= Tests for Chapter 1 =========== --

-- | Test that using @updateSquare@ places one @WordPut@
--   element on the board in the right place,
prop_updateSquare :: Gen Bool 
prop_updateSquare = do
  (p,t) <- genWordPutElement
  let b = updateSquare newBoard (p,t) 
  pure $ Just t == b ! p

-- | Test that using @updateBoard@ puts a @WordPut@ on the
--   board in the right place.
prop_updateBoard :: Gen Bool 
prop_updateBoard = do
  (p,t) <- genWordPutStart
  dir <- genDir
  let inc = if dir == HZ then incCol else incRow
  size <- choose (3,9) :: Gen Int
  let wp = take size (iterate (first inc) (p,t))
      b = updateBoard wp newBoard
      f = if dir == HZ then wordOnRow else wordOnCol 
  pure $ wp == f b p
