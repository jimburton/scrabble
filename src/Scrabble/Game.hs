{-# LANGUAGE Rank2Types #-}
{-|
Module      : Scrabble.Game
Description : Functions for playing a game of scrabble (for a human player).
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for playing a game of scrabble (for a human player).
-}
module Scrabble.Game
  ( newGame
  , updateBoard )
  where

import System.Random
import Prelude hiding (Word)
import Data.Text (Text)
import Control.Lens ((^.),(&),(.~),(%~),Lens')
import Scrabble.Types
  ( Game(..)
  , turn, player1, player2
  , Turn(..)
  , Player(..)
  , score
  , Dict )
import Scrabble.Board
  ( newBoard
  , updateBoard )
import Scrabble.Bag
  ( newBag
  , fillRack )

-- ============= Functions for playing the game =============== --

-- | Start a new game.
newGame :: Text   -- ^ Name of Player 1
        -> Text   -- ^ Name of Player 2
        -> StdGen   -- ^ The random generator
        -> Dict -- ^ The dictionary
        -> Game
newGame p1Name p2Name theGen d = 
  let (rack1, bag1, gen')  = fillRack [] newBag theGen
      p1 = Player { _name  = p1Name
                  , _rack  = rack1
                  , _score = 0
                  , _isAI  = False }
      (rack2, bag2, gen'') = fillRack [] bag1 gen'
      p2 = Player { _name  = p2Name
                  , _rack  = rack2
                  , _score = 0
                  , _isAI  = False }
      g  = Game { _board     = newBoard
                , _bag       = bag2
                , _player1   = p1
                , _player2   = p2
                , _turn      = P1
                , _gen       = gen''
                , _firstMove = True
                , _dict      = d
                , _gameOver  = False
                , _lastMovePass = False } in
    g

