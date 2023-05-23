-- |
-- Module      : Test.Chapter5
-- Description : Tests relating to Chapter 5.
-- Maintainer  : jimburton1@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
--
module Test.Chapter5 (prop_AIGame)
  where

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick)
import Control.Monad.IO.Class (liftIO)
import System.Random (getStdGen)
import Control.Lens ((^.))
import Scrabble.Types
  ( Evaluator(..)
  , turn, player2, isAI
  , Turn(..)
  , MoveResult(..) )
import Scrabble.Game.Game (move)
import Scrabble.Game.AI (moveAI) 
import Scrabble.Board.Pretty()
import Scrabble.Game.Validation (valGameRules)  
import Scrabble.Lang.Dict (englishDictionary)
import Test.Gen
  ( genGameAI
  , p1Word )

-- * Tests for Chapter 5

-- | Test @test beginning an AI game.
prop_AIGame :: Property 
prop_AIGame = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGameAI gen d
  assert (g ^. (player2 . isAI))
  let wp = p1Word g
  case move valGameRules g wp [] of
    Ev (Right (g',_)) -> case moveAI g' of
                          Ev (Right (g'',mr)) -> do assert (mrScore mr > 0)
                                                    assert (g'' ^. turn == P1)
                          Ev (Left _) -> assert False
    Ev (Left _)  -> assert False
