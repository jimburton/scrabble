{-|
Module      : Scrabble.Lang.Word
Description : Words for the Scrabble game.
Maintainer  : jimburton1@gmail.com
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

import Prelude hiding (Word)
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Scrabble.Lang.Letter
  ( letterFromChar
  , letterToChar )
import Scrabble.Types
  ( Word
  , WordPut )

-- ===== Words ===== --

-- | Stringify a @Word@.
wordToString :: Word -> String
wordToString = fmap letterToChar

-- | Convert a @String@ to @Word@.
stringToWord :: String -> Maybe Word
stringToWord s = sequence $ letterFromChar <$> s

-- | Convert a @Text@ to @Word@
textToWord :: Text -> Word
textToWord  t = fromJust $ stringToWord (T.unpack t)

-- | Convert a @Word@ to @Text@
wordToText :: Word -> Text
wordToText w = T.pack (wordToString w)

-- | Convert a @WordPut@ to @Text@
wordPutToText :: WordPut -> Text
wordPutToText = wordToText . map (\(_,(l,_)) -> l)
