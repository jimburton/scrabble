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
                     , words :: [WordPut]
                     } deriving (Show, Eq)

type Turn = Player

type Bag = [Tile]

type Board = Array Int (Array Int (Maybe Tile))

type Game = (Board, Bag, Player, Player)

data Bonus = Word Int | Letter Int

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

