{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-|
Module      : Scrabble.Types
Description : Types for the scrabble library.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Types used by most modules in the scrabble library.
-}
module Scrabble.Types
  ( Letter(..)
  , Word
  , Board
  , Pos
  , WordPut
  , Dir(..)
  , Rack
  , Bonus(..)
  , Player(..)
  , Turn(..)
  , Game(..)
  , Bag
  , Dict
  , Playable
  , Freedom
  , FreedomDir(..)
  , PosTransform
  , Evaluator(..)
  , Validator
  , Tile )

where

import Prelude hiding ( Word )
import Data.Array
import Data.Trie.Text ( Trie )
import Data.Text ( Text )
import qualified Data.Map as Map
import System.Random ( StdGen )
import Data.Aeson
  ( FromJSON
  , ToJSON )
import GHC.Generics

-- ============ Types for the application ================ --

-- | Letters.
data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Show, Read, Enum, Eq, Ord, Generic, FromJSON, ToJSON)

-- | A tile is a letter and a value.
type Tile = (Letter,Int)

-- | A word is a list of letters. 
type Word = [Letter]

-- | The board, a 2D array of Maybe letters and their scores.
type Board = Array (Int,Int) (Maybe Tile)

-- | A position on the board.
type Pos = (Int, Int)

-- | Transform a position on the board
type PosTransform = Pos -> Pos

-- | A word placed on the board (tiles plus positions).
type WordPut = [(Pos, Tile)]

-- | A direction on the board (row or column).
data Dir = HZ -- ^ The horizontal direction.
         | VT -- ^ The vertical direction.
         deriving (Show, Read, Eq)

-- | A rack is a list of letters.
type Rack = [Letter]

-- | The datatype of bonuses on the board.
data Bonus = W2 -- ^ Double word bonus.
           | W3 -- ^ Triple word bonus.
           | L2 -- ^ Double letter bonus.
           | L3 -- ^ Triple letter bonus.
  deriving Show

-- | A player has a name, a rack and a score, and is either an interactive or an AI player.
data Player = Player { name  :: Text -- ^ The name of the player.
                     , rack  :: Rack -- ^ The rack.
                     , score :: Int  -- ^ The score.
                     , isAI  :: Bool -- ^ True if this player is the AI player.
                     } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Which player's turn it is within the game. 
data Turn = P1 -- ^ Player 1.
          | P2 -- ^ Player 2.
          deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

-- | The bag is a list of letters.
type Bag = [Letter]

-- | The dictionary is stored in a trie.
type Dict = Trie ()

-- | A game is comprised of all the state that is needed to play a game. 
data Game = Game { board     :: Board    -- ^ The board
                 , bag       :: Bag      -- ^ The bag.
                 , player1   :: Player   -- ^ Player 1.
                 , player2   :: Player   -- ^ Player 2.
                 , turn      :: Turn     -- ^ Which player's turn it is.
                 , gen       :: StdGen   -- ^ The StdGen for all things random.
                 , firstMove :: Bool     -- ^ Is it the first move?
                 , dict      :: Dict     -- ^ The dictionary.
                 , gameOver  :: Bool     -- ^ Is the game over?
                 , playable  :: Playable -- ^ The map of playable positions.
                 , lastMovePass :: Bool  -- ^ Was the last move a pass?
                 }

-- | A direction on the board (up, down, left or right)
data FreedomDir = UpD     -- ^ The Up direction.
                | DownD   -- ^ The Down direction.
                | LeftD   -- ^ The Left direction.
                | RightD  -- ^ The Right direction.
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

-- | A Freedom is a direction and a distance.
type Freedom = (FreedomDir, Int)

-- | The map of all playable positions on the board, used by the AI player.
type Playable = Map.Map Pos (Letter, [Freedom])

-- | The evaluator of scrabble computations. Wraps up an @Either@ value
--   for error reporting.
newtype Evaluator a = Ev (Either Text a)

-- | Validator is the type of functions that validate words to be played.
--   It returns unit or fails with an error message.
type Validator = [WordPut] -> Game -> Evaluator ()




