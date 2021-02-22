module Scrabble.Types
  where

data Tile = Tile Char Int deriving (Show, Eq)

type Rack = [Tile]

type Score = Int

type WordPut = [((Int, Int), Tile)]

data Player = Player { rack :: Rack
                     , words :: [WordPut]
                     } deriving (Show, Eq)

type Turn = Player

type Bag = [Tile]

type Board = [[Maybe Tile]]

type Game = (Board, Bag, Player, Player)

data Bonus = Word Int | Letter Int

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

