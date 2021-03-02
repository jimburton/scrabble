module Scrabble.Dict.Word
  ( Word
  , wordToString
  , stringToWord
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
stringToWord :: String -> Maybe Word
stringToWord s = sequence $ letterFromChar <$> s

textToWord :: Text -> Word
textToWord  t = fromJust $ stringToWord (T.unpack t)

wordToText :: Word -> Text
wordToText w = T.pack (wordToString w)
