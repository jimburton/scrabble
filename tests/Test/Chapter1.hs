module Test.Chapter1
  where

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick)
import Test.Gen ( genWordPutElement, genWordPut )
import Data.Array ( (!) )
import Control.Lens ((^.))
import System.Random (getStdGen)
import Control.Monad.IO.Class (liftIO)

import Scrabble.Types (Dir(..))
import Scrabble.Board
  ( newBoard
  , updateSquare
  , updateBoard
  , wordOnRow
  , wordOnCol
  , getDirection )
import Scrabble.Dict (englishDictionary)
import Scrabble.Pretty() -- for the Show instance of Game

-- ============= Tests for Chapter 1 =========== --

-- | Test that using @updateSquare@ places one @WordPut@
--   element on the board in the right place,
prop_updateSquare :: Property 
prop_updateSquare = monadicIO $ do
  wpe <- pick genWordPutElement
  let b = updateSquare newBoard wpe
  assert $ Just (snd wpe) == b ! fst wpe

-- | Test that using @updateBoard@ puts a @WordPut@ on the
--   board in the right place.
prop_updateBoard :: Property
prop_updateBoard = monadicIO $ do
  wp  <- pick genWordPut
  let b = updateBoard wp newBoard 
      f = if getDirection wp == HZ then wordOnRow else wordOnCol 
  assert $ wp == f b (fst $ head wp)
