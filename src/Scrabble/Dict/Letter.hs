module Scrabble.Dict.Letter
  ( Letter(..)
  , toChar
  , letterFromChar
  , charToLetterMap
  , scoreLetter )
  where

import qualified Data.Map as Map
import           Data.Map    ( Map )
import           Data.Maybe  ( fromJust )
import           Data.Tuple  ( swap )

-- ===== Letters ===== --

data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Enum, Eq, Ord)

-- | Convert a Char to a Letter, if the Char is a valid Letter (A-Z).
letterFromChar :: Char -> Maybe Letter
letterFromChar c = Map.lookup c charToLetterMap

-- | Convert a Letter back into a Char. Always valid.
toChar :: Letter -> Char
toChar l = fromJust $ Map.lookup l letterToCharMap

instance Show Letter where
  show l = [toChar l]

-- private value.
letterToCharList :: [(Letter,Char)]
letterToCharList = [
  (A, 'A'), (B, 'B'), (C, 'C'), (D, 'D'), (E, 'E'), (F, 'F'), (G, 'G'),
  (H, 'H'), (I, 'I'), (J, 'J'), (K, 'K'), (L, 'L'), (M, 'M'), (N, 'N'),
  (O, 'O'), (P, 'P'), (Q, 'Q'), (R, 'R'), (S, 'S'), (T, 'T'), (U, 'U'),
  (V, 'V'), (W, 'W'), (X, 'X'), (Y, 'Y'), (Z, 'Z'), (Blank, '_') ]

-- private value.
letterToCharMap :: Map Letter Char
letterToCharMap = Map.fromList letterToCharList

-- | Mapping chars to letters.
charToLetterMap :: Map Char Letter
charToLetterMap = Map.fromList (swap <$> letterToCharList)

-- private value.
letterToScoreList :: [(Letter,Int)]
letterToScoreList = [
  (A, 1), (B, 3), (C, 3), (D, 2), (E, 1), (F, 4), (G, 2),
  (H, 4), (I, 1), (J, 8), (K, 5), (L, 1), (M, 3), (N, 1),
  (O, 1), (P, 3), (Q, 10), (R, 1), (S, 1), (T, 1), (U, 1),
  (V, 4), (W, 4), (X, 8), (Y, 4), (Z, 10), (Blank, 0) ]

-- private value.
letterToScoreMap :: Map Letter Int
letterToScoreMap = Map.fromList letterToScoreList

-- | Find the score of a letter.
scoreLetter :: Letter -> Int
scoreLetter = fromJust . flip Map.lookup letterToScoreMap

