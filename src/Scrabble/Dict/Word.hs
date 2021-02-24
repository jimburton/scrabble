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
{- ===== Words ===== -}

type Word = [Letter]

wordToString :: Word -> String
wordToString = fmap toChar

wordFromString :: String -> Maybe Word
wordFromString s = sequence $ letterFromChar <$> s
