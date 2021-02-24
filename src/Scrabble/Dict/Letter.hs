module Scrabble.Dict.Letter
  ( Letter(..)
  , toChar
  , letterFromChar
  , charToLetterMap )
  where

import qualified Data.Map as Map
import           Data.Map    ( Map )
import           Data.Maybe  ( fromJust )
import           Data.Tuple  ( swap )

{- ===== Letters ===== -}

data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Enum, Eq, Ord)

instance Show Letter where
  show l = [toChar l]

-- | Convert a Char to a Letter, if the Char is a valid Letter (A-Z).
letterFromChar :: Char -> Maybe Letter
letterFromChar c = Map.lookup c charToLetterMap

-- | Convert a Letter back into a Char. Always valid.
toChar :: Letter -> Char
toChar l = fromJust $ Map.lookup l letterToCharMap

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

charToLetterMap :: Map Char Letter
charToLetterMap = Map.fromList (swap <$> letterToCharList)
