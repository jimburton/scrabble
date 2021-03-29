{-|
Module      : Scrabble.Lang.Dict
Description : Functions relating to the dictionary for the Scrabble game.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions relating to the dictionary for the Scrabble game. The dictionary is stored as
a @Data.Text.Trie@. Currently hardcoded for the English Scrabble dictionary provided
in the repository.
-}
module Scrabble.Lang.Dict (englishDictionary)
  where

import Prelude hiding (Word)
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Trie.Text as Trie
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (nub, permutations)
import Control.Monad (filterM)
import Data.Tuple (swap)
import Scrabble.Types
  ( Dict
  , Letter(..)
  , WordPut
  , Word)

-- * Dictionary

-- Reads in a dictionary of Scrabble words from the given file.
readDictionary :: FilePath -> IO Dict
readDictionary dict = do
  ls <- lines <$> readFile dict
  pure (Trie.fromList [(T.pack (map toUpper l), ()) | l <- ls])

-- ===== English Dictionary ===== --

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt" -- "./dict/english2.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionary :: IO Dict
englishDictionary = readDictionary englishDictionaryPath

