module Scrabble.Dict.Word
  ( Word
  , wordToString
  , wordFromString )
  where

import Scrabble.Dict.Letter
  ( Letter
  , letterFromChar
  , toChar )
import Prelude hiding  ( Word )

-- ===== Words ===== --

type Word = [Letter]

-- | Stringify a word.
wordToString :: Word -> String
wordToString = fmap toChar

-- | Turn a string into a word.
wordFromString :: String -> Maybe Word
wordFromString s = sequence $ letterFromChar <$> s
