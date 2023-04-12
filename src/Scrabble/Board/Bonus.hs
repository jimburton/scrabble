{-|
Module      : Scrabble.Board.Bonus
Description : Functions for bonuses the scrabble library.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

Contains the @bonusMap@ mapping positions to bonuses.
-}
module Scrabble.Board.Bonus
  ( bonusMap )
  where

import Prelude hiding (Word)
import Data.Map (Map)
import qualified Data.Map as Map
import Scrabble.Types
  ( Pos
  , Bonus(..) )

-- * Bonuses

-- Data for the bonus map.
bonusSquaresList :: [(Pos, Bonus)] -- ((Row, Column), Bonus)
bonusSquaresList =
  [((0, 0),    W3), ((0, 3),   L2)
  , ((0, 7),   W3), ((0, 11),  L2)
  , ((0, 14),  W3), ((1, 1),   W2)
  , ((1, 5),   L3), ((1, 9),   L3)
  , ((1, 13),  W2), ((2, 2),   W2)
  , ((2, 6),   L2), ((2, 8),   L2)
  , ((2, 12),  W2), ((3, 0),   L2)
  , ((3, 3),   W2), ((3, 7),   L2)
  , ((3, 11),  W2), ((3, 14),  L2)
  , ((4, 4),   W2), ((4, 10),  W2)
  , ((5, 1),   L3), ((5, 5),   L3)
  , ((5, 9),   L3), ((5, 13),  L3)
  , ((6, 2),   L2), ((6, 6),   L2)
  , ((6, 8),   L2), ((6, 12),  L2)
  , ((7, 0),   W3), ((7, 3),   L2)
  , ((7, 7),   W2), ((7, 11),  L2)
  , ((7, 14),  W3), ((8, 2),   L2)
  , ((8, 6),   L2), ((8, 8),   L2)
  , ((8, 12),  L2), ((9, 1),   L3)
  , ((9, 5),   L3), ((9, 9),   L3)
  , ((9, 13),  L3), ((10, 4),  W2)
  , ((10, 10), W2), ((11, 0),  L2)
  , ((11, 3),  W2), ((11, 7),  L2)
  , ((11, 11), W2), ((11, 14), L2)
  , ((12, 2),  W2), ((12, 6),  L2)
  , ((12, 8),  L2), ((12, 12), W2)
  , ((13, 1),  W2), ((13, 5),  L3)
  , ((13, 9),  L3), ((13, 13), W2)
  , ((14, 0),  W3), ((14, 3),  L2)
  , ((14, 7),  W3), ((14, 11), L2)
  , ((14, 14), W3)]

-- | Map containing the bonus squares on the board.
bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList
