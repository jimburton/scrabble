{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Scrabble.Types
Description : Types for the scrabble library.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Types used across the scrabble library.
-}
module Scrabble.Types
  ( Letter(..)
  , Word
  , Board
  , Pos
  , WordPut
  , Rack
  , Bonus(..)
  , Bag
  , Dict
  , Tile
  , Dir(..)
  , PosTransform
  , Player(..)
  , name, rack, score, isAI -- lenses for Player.
  , Game(..)
  , board, bag, player1, player2, turn, gen, firstMove, --lenses for Game
    dict, gameOver, lastMovePass -- lenses for Game.
  , Turn(..)
  )

where

import Prelude hiding (Word)
import Data.Array
import Data.Trie.Text (Trie)
import Data.Text (Text)
import System.Random (StdGen)
import Lens.Simple 

-- ============ Types for the application ================ --

-- | Letters.
data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Show, Read, Enum, Eq, Ord)

-- | A tile is a pair of a letter and a value.
type Tile = (Letter,Int)

-- | A word is a list of letters. 
type Word = [Letter]

-- | The board, a 2D array of Maybe letters and their scores.
type Board = Array (Int,Int) (Maybe Tile)

-- | A position on the board.
type Pos = (Int, Int)

-- | A word placed on the board (tiles plus positions).
type WordPut = [(Pos, Tile)]

-- | A direction on the board (row or column).
data Dir = HZ -- ^ The horizontal direction.
         | VT -- ^ The vertical direction.
         deriving (Show, Read, Eq)

-- | Transform a position on the board
type PosTransform = Pos -> Pos

-- | A rack is a list of letters.
type Rack = [Letter]

-- | The datatype of bonuses on the board.
data Bonus = W2 -- ^ Double word bonus.
           | W3 -- ^ Triple word bonus.
           | L2 -- ^ Double letter bonus.
           | L3 -- ^ Triple letter bonus.
  deriving Show

-- | The bag is a list of letters.
type Bag = [Letter]

-- | The dictionary is stored in a trie.
type Dict = Trie ()

-- | A player has a name, a rack and a score, and is either an interactive or an AI player.
data Player = Player { _name  :: Text -- ^ The name of the player.
                     , _rack  :: Rack -- ^ The rack.
                     , _score :: Int  -- ^ The score.
                     , _isAI  :: Bool -- ^ True if this player is the AI player.
                     } deriving (Show, Eq)
-- | Make lenses for Player
$(makeLenses ''Player)

-- | Which player's turn it is within the game. 
data Turn = P1 -- ^ Player 1.
          | P2 -- ^ Player 2.
          deriving (Show, Read, Eq)

-- | A game is comprised of all the state that is needed to play a game. 
data Game = Game { _board     :: Board    -- ^ The board
                 , _bag       :: Bag      -- ^ The bag.
                 , _player1   :: Player   -- ^ Player 1.
                 , _player2   :: Player   -- ^ Player 2.
                 , _turn      :: Turn     -- ^ Which player's turn it is.
                 , _gen       :: StdGen   -- ^ The StdGen for all things random.
                 , _firstMove :: Bool     -- ^ Is it the first move?
                 , _dict      :: Dict     -- ^ The dictionary.
                 , _gameOver  :: Bool     -- ^ Is the game over?
                 , _lastMovePass :: Bool  -- ^ Was the last move a pass?
                 }
-- | Make lenses for Game.
$(makeLenses ''Game)
