{-|
Module      : Scrabble.Lang.Word
Description : Words for the Scrabble game.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Words for the Scrabble game.
-}
module Scrabble.Lang.Word
  ( wordToString
  , stringToWord
  , textToWord
  , wordToText
  , wordPutToText )
  where

import Prelude hiding  ( Word )
import Data.Text       ( Text )
import Data.Maybe      ( fromJust )
import qualified Data.Text as T
import Scrabble.Lang.Letter
  ( letterFromChar
  , toChar )
import Scrabble.Types
  ( Word
  , WordPut )

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

wordPutToText :: WordPut -> Text
wordPutToText = wordToText . map (\(_,(l,_)) -> l)
