{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Scrabble.Board.Pretty
Description : Pretty printing of Scrabble boards.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for pretty-printing a Scrabble board.
-}
module Scrabble.Board.Pretty
  ( showBoard
  , showGame
  , showPlayer )
  where

import Data.Array ( elems )
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.List.Split (chunksOf)
import Control.Lens ((^.))
import Scrabble.Board.Bonus (bonusMap)
import Scrabble.Types
  ( Board
  , Player(..)
  , score, rack, name
  , Game
  , board, player1, player2
  , Letter
  , Tile )
import Scrabble.Lang.Letter
  ( scoreLetter
  , letterToChar ) 

-- * Show instances for Player and Game

instance Show Game where
  show g = T.unpack (showBoard False (g ^. board)) ++ "\n"
           ++ show (g ^. player1) ++ "\n" ++ show (g ^. player2)
           
-- * Functions for turning boards into text

-- Get all rows from the board
rows :: Board -> [[Maybe Tile]]
rows b = chunksOf 15 (elems b) 

-- | Textify a board.
showBoard :: Bool  -- ^ Whether to show bonus squares.
          -> Board -- ^ The board.
          -> Text  -- ^ The text representation of the board.
showBoard printBonuses b = topNumbers <> top <> showRows <> bottom where
  showRows      = T.intercalate "\n" (zipWith showRow [0..14] (rows b)) <> "\n"
  showRow     i r = showI i <> "|" <>
                    T.concat (zipWith (showSquare i) [0..14] r)
  showSquare :: Int -> Int -> Maybe (Letter,Int) -> Text
  showSquare i c s = case s of
                       Nothing    ->
                         if printBonuses
                         then case Map.lookup (i,c) bonusMap of
                                Nothing -> "  |"
                                Just b' -> T.pack (show b') <> "|"
                         else "  |"
                       Just (t,_) -> T.pack [' ', letterToChar t, '|']
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
         -> Text   -- ^ The text representation of the game.
showGame printBonuses b p = showBoard printBonuses b <> showPlayer p

-- | Textify a player.
showPlayer :: Player -- ^ The player.
           -> Text   -- ^ The text representation of the player.
showPlayer p = top <> playerLine <> rackLine <> bottom where
  line       :: Char -> Text
  line     c = T.pack (replicate 46 c) <> "\n"
  top        = "\n" <> line '*'
  playerLine = p ^. name <> " (" <> T.pack (show (p ^. score)) <> ")\n"
  rackLine   = let strs   = map (T.pack . (:"") . letterToChar) (p ^. rack)
                   scores = map (T.pack . show . scoreLetter) (p ^. rack) in
                 T.intercalate ", " strs <> "\n"
                 <> T.intercalate ", " scores <> "\n"
  bottom     = line '*'
