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
  , Dir(..)
  , Rack
  , Bonus(..)
  , Bag
  , Dict
  , Tile)

where

import Prelude hiding (Word)
import Data.Array
import Data.Trie.Text (Trie)

-- ============ Types for the application ================ --

-- | Letters.
data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Show, Read, Enum, Eq, Ord)

-- | A tile is a letter and a value.
type Tile = (Letter,Int)

-- | A word is a list of letters. 
type Word = [Letter]

-- | The board, a 2D array of Maybe letters and their scores.
type Board = Array (Int,Int) (Maybe Tile)

-- | A position on the board.
type Pos = (Int, Int)

-- | A word placed on the board (tiles plus positions).
type WordPut = [(Pos, Tile)]

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
