{-# LANGUAGE OverloadedStrings #-}
module Scrabble.Show
  ( showBoard
  , showGame
  , showPlayer )
  where

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
showBoard printBonuses b = topNumbers <> top <> showRows <> bottom where
  showRows      = T.intercalate "\n" (zipWith showRow [0..14] (elems b)) <> "\n"
  showRow     i r = showI i <> "|" <>
                    T.concat (zipWith (showSquare i) [0..14] (elems r))
  showSquare :: Int -> Int -> Maybe (Letter,Int) -> Text
  showSquare i c s = case s of
                       Nothing    ->
                         if printBonuses
                         then case Map.lookup (i,c) bonusMap of
                                Nothing -> "  |"
                                Just b' -> T.pack (show b') <> "|"
                         else "  |"
                       Just (t,_) -> T.pack [' ', toChar t, '|']
  topNumbers    = "  |" <> T.concat (map (\i -> showI i <> "|") [0..14]) <> "\n"
  showI         :: Int -> Text
  showI i       = if i < 10 then " " <> T.pack (show i) else T.pack (show i)
  top           = line '-'
  bottom        = line '-'
  line        c = T.pack (replicate 48 c) <> "\n"

-- | Textify the board and the current player.
showGame :: Bool   -- ^ Whether to show bonus squares.
         -> Board  -- ^ The board.
         -> Player -- ^ The player.
         -> Text
showGame printBonuses b p = showBoard printBonuses b <> showPlayer p

-- | Textify a player.
showPlayer :: Player -- ^ The player.
           -> Text
showPlayer p = top <> playerLine <> rackLine <> bottom where
  line       :: Char -> Text
  line     c = T.pack (replicate 46 c) <> "\n"
  top        = "\n" <> line '*'
  playerLine = name p <> " (" <> T.pack (show (score p)) <> ")\n"
  rackLine   = let strs   = map (T.pack . show) (rack p)
                   scores = map (T.pack . show . scoreLetter) (rack p) in
                 T.intercalate ", " strs <> "\n"
                 <> T.intercalate ", " scores <> "\n"
  bottom     = line '*'

