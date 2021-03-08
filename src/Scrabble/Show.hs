{-# LANGUAGE OverloadedStrings #-}
module Scrabble.Show
  ( showBoard
  , showGame
  , showPlayer )
  where

import Data.List      ( intercalate )
import Data.Array
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text ( Text )
import Scrabble.Board.Board
  ( bonusMap )
import Scrabble.Types
  ( Board
  , Player(..)
  , Letter )
import Scrabble.Lang.Letter
  ( toChar
  , scoreLetter ) 

-- =============== Functions for turning boards into text =========== --

-- | Textify a board.
showBoard :: Bool  -- ^ Whether to show bonus squares.
          -> Board -- ^ The board.
          -> Text
showBoard printBonuses b = topNumbers `T.append` top `T.append` showRows `T.append` bottom where
  showRows      = T.intercalate "\n" (zipWith showRow [0..14] (elems b)) `T.append` "\n"
  showRow     i r = showI i `T.append` "|" `T.append`
                    T.concat (zipWith (showSquare i) [0..14] (elems r))
  showSquare :: Int -> Int -> Maybe (Letter,Int) -> Text
  showSquare i c s = case s of
                       Nothing    ->
                         if printBonuses
                         then case Map.lookup (i,c) bonusMap of
                                Nothing -> "  |"
                                Just b' -> (T.pack $ show b') `T.append` "|"
                         else "  |"
                       Just (t,_) -> T.pack [' ', toChar t, '|']
  topNumbers    = "  |" `T.append` (T.concat (map (\i -> showI i `T.append` "|") [0..14])) `T.append` "\n"
  showI         :: Int -> Text
  showI i       = if i < 10 then " " `T.append` (T.pack $ show i) else (T.pack $ show i)
  top           = line '-'
  bottom        = line '-'
  line        c = (T.pack $ replicate 48 c) `T.append` "\n"

-- | Textify the board and the current player.
showGame :: Bool   -- ^ Whether to show bonus squares.
         -> Board  -- ^ The board.
         -> Player -- ^ The player.
         -> Text
showGame printBonuses b p = showBoard printBonuses b `T.append` showPlayer p

-- | Textify a player.
showPlayer :: Player -- ^ The player.
           -> Text
showPlayer p = top `T.append` playerLine `T.append` rackLine `T.append` bottom where
  line       :: Char -> Text
  line     c = (T.pack $ replicate 46 c) `T.append` "\n"
  top        = "\n" `T.append` line '*'
  playerLine = name p `T.append` " (" `T.append` (T.pack $ show (score p)) `T.append` ")\n"
  rackLine   = let strs   = map (T.pack . show) (rack p)
                   scores = map (T.pack . show . scoreLetter) (rack p) in
                 T.intercalate ", " strs `T.append` "\n"
                 `T.append` T.intercalate ", " scores `T.append` "\n"
  bottom     = line '*'

