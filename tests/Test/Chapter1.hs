{-# LANGUAGE ScopedTypeVariables  #-}
module Test.Chapter1
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

import Scrabble.Types
  ( Dir(..)
  , board
  , Evaluator(..) )
import Scrabble.Board.Board
  ( updateSquare
  , incCol
  , incRow
  , updateBoard
  , wordOnRow
  , wordOnCol )
import Scrabble.Lang.Dict (englishDictionary)
import Scrabble.Board.Pretty() -- for the Show instance of Game

-- ============= Tests for Chapter 1 =========== --

-- | Test that using @updateSquare@ places one @WordPut@
--   element on the board in the right place,
prop_updateSquare :: Property 
prop_updateSquare = monadicIO $ do
  (p,t) <- pick $ genWordPutElement
  gen   <- liftIO $ getStdGen
  d     <- liftIO $ englishDictionary
  g     <- pick $ genGame gen d
  let b = updateSquare (g ^. board) (p,t) 
  assert $ Just t == b ! p

-- | Test that using @updateBoard@ puts a @WordPut@ on the
--   board in the right place.
prop_updateBoard :: Property
prop_updateBoard = monadicIO $ do
  (p,t) <- pick $ genWordPutStart
  dir   <- pick $ genDir
  let inc = if dir == HZ then incCol else incRow
  (size :: Int)  <- pick $ choose (3,9)
  gen   <- liftIO $ getStdGen
  d     <- liftIO $ englishDictionary
  g     <- pick $ genGame gen d
  let wp = take size (iterate (first inc) (p,t))
      Ev (Right g') = updateBoard wp g
      f = if dir == HZ then wordOnRow else wordOnCol 
  assert $ wp == f (g' ^. board) p
