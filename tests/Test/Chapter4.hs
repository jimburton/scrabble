module Test.Chapter4
  ( prop_pass
  , prop_swap
  , prop_move )
  where

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick)
import Control.Monad.IO.Class (liftIO)
import System.Random (getStdGen)
import Control.Lens ((^.))
import Data.List (isInfixOf)
import Scrabble.Types
  ( Evaluator(..)
  , player1, rack, turn, bag
  , Turn(..))
import Scrabble.Game.Game
  ( pass
  , swap
  , move ) 
import Scrabble.Board.Board (wordPutToWord)
import Scrabble.Board.Pretty()
import Scrabble.Game.Validation (valGameRules)  
import Scrabble.Lang.Dict (englishDictionary)
import Test.Gen
  ( genGame
  , p1Word )

-- ========= Tests for Chapter 4 ========== --

-- | Test @pass@.
prop_pass :: Property 
prop_pass = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  case pass g of
    Ev (Right g') -> assert $ (g' ^. turn) == P2
    Ev (Left _)  -> assert False

-- | Test @swap@.
prop_swap :: Property 
prop_swap = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let (swaps,keeps) = splitAt 5 (g ^. (player1 .rack))
  case swap swaps g  of
    Ev (Right g') -> do assert $ (g' ^. turn) == P2
                        assert $ length (g ^. bag) == length (g' ^. bag)
                        assert $ keeps `isInfixOf` (g' ^. (player1 . rack))
    Ev (Left _)  -> assert False

-- | Test @move@.
prop_move :: Property 
prop_move = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let wp = p1Word g
  case move valGameRules g wp [] of
    Ev (Right (g',(ws,_))) -> do assert $ (g' ^. turn) == P2
                                 assert $ wordPutToWord wp == head ws
                                 assert $ length (g ^. bag) == length (g' ^. bag) + 7
    Ev (Left _)  -> assert False

