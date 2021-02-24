module Scrabble.Word ( Word
                     , wordToString
                     , wordFromString )
  where

import Scrabble.Letter ( Letter
                       , letterFromChar
                       , toChar )
import Prelude hiding  ( Word )
{- ===== Words ===== -}

type Word = [Letter]

wordToString :: Word -> String
wordToString = fmap toChar

wordFromString :: String -> Maybe Word
wordFromString s = sequence $ letterFromChar <$> s
