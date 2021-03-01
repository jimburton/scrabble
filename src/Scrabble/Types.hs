module Scrabble.Types
  ( Letter
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
  , DictTrie ) 

where

import Prelude hiding ( Word )
import Data.Array
import Data.Trie.Text ( Trie ) 
import System.Random ( StdGen )
import Scrabble.Dict.Letter ( Letter ) 

-- ============ Types for the application ================ --

-- Letter is declared in Scrabble.Dict.Letter

type Word = [Letter]

type Board = Array Int (Array Int (Maybe Letter))

type Pos = (Int, Int)

type WordPut = [(Pos, Letter)]

data Dir = HZ | VT deriving (Show, Read, Eq)

type Rack = [Letter]

data Bonus = Word Int | Letter Int -- ^ The datatype of bonuses

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

data Player = Player { name :: String
                     , rack :: Rack
                     , score :: Int
                     } deriving (Show, Eq)

data Turn = P1 | P2 deriving (Show, Eq)

data Game = Game { board   :: Board
                 , bag     :: Bag
                 , player1 :: Player
                 , player2 :: Player
                 , turn    :: Turn
                 , gen     :: StdGen
                 , firstMove :: Bool }

type Bag = [Letter]

type DictTrie = Trie Bool

