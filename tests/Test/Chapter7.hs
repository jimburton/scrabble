module Test.Chapter7 (prop_cliGame)
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

-- ========= Tests for Chapter 5 ========== --

-- | Test @test beginning a CLI game.
prop_cliGame :: Property 
prop_cliGame = monadicIO $ return ()
