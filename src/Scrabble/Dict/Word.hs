module Scrabble.Dict.Word
  ( Word
  , wordToString
  , wordFromString )
  where

import Prelude hiding  ( Word )
import Scrabble.Dict.Letter
  ( letterFromChar
  , toChar )
import Scrabble.Types ( Word )

-- ===== Words ===== --

-- | Stringify a word.
wordToString :: Word -> String
wordToString = fmap toChar

-- | Turn a string into a word.
wordFromString :: String -> Maybe Word
wordFromString s = sequence $ letterFromChar <$> s
