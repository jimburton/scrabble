module Scrabble.Types
  where

import Data.Array
import Prelude hiding (words)

data Tile = Tile Char Int deriving Eq

instance Show Tile where
  show (Tile c i) = show c ++ " (" ++ show i ++ ")"

type Pos = (Int, Int)

type Rack = [Tile]

type Score = Int

type WordPut = [(Pos, Tile)]

data Player = Player { name :: String
                     , rack :: Rack
                     , score :: Int
                     } deriving (Show, Eq)

type Bag = [Tile]

type Board = Array Int (Array Int (Maybe Tile))

data Turn = P1 | P2 deriving (Show, Eq)

data Game = Game { board   :: Board
                 , bag     :: Bag
                 , player1 :: Player
                 , player2 :: Player
                 , turn    :: Turn }

data Bonus = Word Int | Letter Int

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

