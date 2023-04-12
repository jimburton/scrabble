{-|
Module      : Scrabble.Lang.Dict
Description : Functions relating to the dictionary for the Scrabble game.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

Functions relating to the dictionary for the Scrabble game. The dictionary is stored as
a @Data.Text.Trie@. Currently hardcoded for the English Scrabble dictionary provided
in the repository.
-}
module Scrabble.Lang.Dict (englishDictionary)
  where

import Data.Char (toUpper)
import Codec.Binary.UTF8.String (encode)
import qualified Data.ByteString as B
import qualified Data.Trie as Trie
import Scrabble.Types (Dict)

-- * Dictionary

-- Reads in a dictionary of Scrabble words from the given file.
readDictionary :: FilePath -> IO Dict
readDictionary dict = do
  ls <- lines <$> readFile dict
  pure (Trie.fromList [(B.pack $ encode (map toUpper l), ()) | l <- ls])

-- * English Dictionary

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt" -- "./dict/english2.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionary :: IO Dict
englishDictionary = readDictionary englishDictionaryPath
