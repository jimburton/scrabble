module Scrabble.Dict.Word
  ( Word
  , wordToString
  , wordFromString
  , textToWord
  , wordToText )
  where

import Prelude hiding  ( Word )
import Data.Text       ( Text )
import Data.Maybe      ( fromJust )
import qualified Data.Text as T
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

textToWord :: Text -> Word
textToWord  t = fromJust $ wordFromString (T.unpack t)

wordToText :: Word -> Text
wordToText w = T.pack (wordToString w)
