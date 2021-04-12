module Test.Chapter1
  where

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick)
import Test.Gen
import Data.Array
import Lens.Simple ((^.))
import System.Random (getStdGen)
import Control.Monad.IO.Class (liftIO)

import Scrabble.Types
  ( Dir(..)
  , board
  , Evaluator(..)
  , Dir(..))
import Scrabble.Board.Board
  ( updateSquare
  , updateBoard
  , wordOnRow
  , wordOnCol
  , getDirection )
import Scrabble.Lang.Dict (englishDictionary)
import Scrabble.Board.Pretty() -- for the Show instance of Game

-- ============= Tests for Chapter 1 =========== --

-- | Test that using @updateSquare@ places one @WordPut@
--   element on the board in the right place,
prop_updateSquare :: Property 
prop_updateSquare = monadicIO $ do
  wpe <- pick genWordPutElement
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let b = updateSquare (g ^. board) wpe
  assert $ Just (snd wpe) == b ! (fst wpe)

-- | Test that using @updateBoard@ puts a @WordPut@ on the
--   board in the right place.
prop_updateBoard :: Property
prop_updateBoard = monadicIO $ do
  wp  <- pick genWordPut
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let Ev (Right g') = updateBoard wp g
      f = if getDirection wp == HZ then wordOnRow else wordOnCol 
  assert $ wp == f (g' ^. board) (fst $ head wp)
