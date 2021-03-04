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
  , DictTrie
  , Playable
  , FreedomDir(..)
  , PosTransform
  , Evaluator(..)
  , Validator )

where

import Prelude hiding ( Word )
import Data.Array
import Data.Trie.Text ( Trie )
import qualified Data.Map as Map
import System.Random ( StdGen )

-- ============ Types for the application ================ --

data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Show, Enum, Eq, Ord)

-- | A word is a list of letters. 
type Word = [Letter]

-- | The board, a 2D array of Maybe letters and their scores.
type Board = Array Int (Array Int (Maybe (Letter, Int)))

-- | A position on the board.
type Pos = (Int, Int)

-- | Transform a position on the board
type PosTransform = Pos -> Pos

-- | A word placed on the board (tiles plus positions).
type WordPut = [(Pos, (Letter, Int))]

-- | A direction on the board (row or column).
data Dir = HZ | VT deriving (Show, Read, Eq)

-- | A rack is a list of letters.
type Rack = [Letter]

-- | The datatype of bonuses on the board.
data Bonus = Word Int | Letter Int 

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

-- | A player has a name, a rack and a score, and is either an interactive or an AI player.
data Player = Player { name  :: String
                     , rack  :: Rack
                     , score :: Int
                     , isAI  :: Bool
                     } deriving (Show, Eq)

-- | Which player's turn it is within the game. 
data Turn = P1 | P2 deriving (Show, Eq)

-- | The bag is a list of letters.
type Bag = [Letter]

-- | The dictionary is stored in a trie.
type DictTrie = Trie Bool

-- | A game is comprised of all the state that is needed to play a game. 
data Game = Game { board     :: Board    -- ^ The board
                 , bag       :: Bag      -- ^ The bag.
                 , player1   :: Player   -- ^ Player 1.
                 , player2   :: Player   -- ^ Player 2.
                 , turn      :: Turn     -- ^ Which player's turn it is.
                 , gen       :: StdGen   -- ^ The StdGen for all things random.
                 , firstMove :: Bool     -- ^ Is it the first move?
                 , dict      :: DictTrie -- ^ The dictionary.
                 , gameOver  :: Bool     -- ^ Is the game over?
                 , playable  :: Playable -- ^ The map of playable positions.
                 , lastMovePass :: Bool  -- ^ Was the last move a pass?
                 }

-- | The map of all playable positions on the board, used by the AI player.
type Playable = Map.Map Pos (Letter, [(FreedomDir, Int)])

-- | A direction on the board (up, down, left or right)
data FreedomDir = UpD | DownD | LeftD | RightD deriving (Show, Read, Eq)

newtype Evaluator a = Ev (Either String a)

-- | Validator is the type of functions that validate words to be played
type Validator = [WordPut] -> Game -> Evaluator Bool




