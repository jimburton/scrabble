{-# LANGUAGE TupleSections #-}
{-|
Module      : Scrabble.Game.Game
Description : Functions for playing a game of scrabble (for a human player).
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for playing a game of scrabble (for a human player).
-}
module Scrabble.Game.Game
  ( Turn(..)
  , newGame
  , newBoard
  , newBag
  , updateBoard
  , getPlayer
  , setPlayer )
  where

import System.Random
import Prelude hiding
  ( Word
  , words )
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Text (Text)
import Lens.Simple ((.~),(^.),(&))
import Scrabble.Types
  ( Game(..)
  , player1, player2, turn 
  , lastMovePass
  , gen
  , bag
  , Turn(..)
  , WordPut
  , Player(..)
  , rack
  , Dict
  , Word
  , Validator )
import Scrabble.Board.Board
  ( newBoard
  , additionalWords
  , updateBoard
  , wordPutToWord )
import Scrabble.Board.Bag
  ( newBag
  , fillRack )
import Scrabble.Evaluator (Evaluator(..))

-- ============= Functions for playing the game =============== --

-- | Start a new game.
newGame :: Text   -- ^ Name of Player 1
        -> Text   -- ^ Name of Player 2
        -> StdGen   -- ^ The random generator
        -> Dict -- ^ The dictionary
        -> Game
newGame p1Name p2Name theGen d = 
  let Ev (Right (rack1, bag1, gen')) = fillRack [] newBag theGen
      p1 = Player { _name  = p1Name
                  , _rack  = rack1
                  , _score = 0
                  , _isAI = False }
      Ev (Right (rack2, bag2, gen'')) = fillRack [] bag1 gen'
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

-- | Update the current player in the game. 
setPlayer :: Game -> Player -> Evaluator Game
setPlayer g p = pure $ g & getPlayer g .~ p 

-- | Get the lens for the current player in the game.
getPlayer :: Functor f => Game -> (Player -> f Player) -> Game -> f Game
getPlayer g = if g ^. turn == P1 then player1 else player2
