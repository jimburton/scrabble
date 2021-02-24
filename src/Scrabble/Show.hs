module Scrabble.Show ( showBoard
                     , showGame
                     , showPlayer
                     , showTurn )
  where

import Data.List      ( intercalate )
import Data.Array
import qualified Data.Map as Map
import Scrabble.Game   ( Game
                       , getPlayer )
import Scrabble.Board.Board  ( Board
                       , Player(..)
                       , bonusMap )
import Scrabble.Letter ( toChar )

showBoard :: Bool -> Board -> String
showBoard printBonuses b = topNumbers ++ top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (zipWith showRow [0..14] (elems b)) ++ "\n"
  showRow     i r = showI i ++ "|" ++ concat (zipWith (showSquare i) [0..14] (elems r)) 
  showSquare i c s = case s of
                       Nothing -> if printBonuses
                                  then case Map.lookup (i,c) bonusMap of
                                         Nothing -> "  |"
                                         Just b' -> show b' ++ "|"
                                  else "  |"
                       Just t -> [' ', toChar t, '|']
  topNumbers    = "  |" ++ concatMap (\i -> showI i ++ "|") [0..14] ++ "\n"
  showI i       = if i < 10 then " " ++ show i else show i
  top           = line '-'
  bottom        = line '-'
  line        c = replicate 48 c ++ "\n"

showGame :: Bool -> Board -> Player -> String
showGame printBonuses b p = showBoard printBonuses b ++ showPlayer p

showPlayer :: Player -> String
showPlayer p = top ++ playerLine ++ rackLine ++ bottom where
  line     c = replicate 46 c ++ "\n"
  top        = "\n" ++ line '*'
  playerLine = name p ++ " (" ++ show (score p) ++ ")\n"
  rackLine   = let strs = map show (rack p) in
                 intercalate ", " (take 5 strs) ++ "\n"
                 ++ intercalate ", " (drop 5 strs) ++ "\n"
  bottom     = line '*'

showTurn :: Game -> String
showTurn g = let p = getPlayer g in
  showPlayer p ++ "Enter WORD ROW COL DIR[H/V]:\n"

