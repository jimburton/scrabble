module Scrabble.Lang.Dict
  ( letterFromChar
  , toChar
  , wordsInDictM
  , wordsInDict
  , englishDictionary
  , dictContainsWord
  , dictContainsPrefix )
  where

import Data.Char        ( toUpper )
import Prelude hiding   ( Word )
import Data.Text        ( Text )
import qualified Data.Text as T
import Data.Monoid ( mconcat )
import qualified Data.Trie.Text as Trie

import Scrabble.Lang.Letter
  ( letterFromChar
  , toChar )
import Scrabble.Evaluator
  ( Evaluator(..)
  , evalBool )
import Scrabble.Types
  ( Dict )

{- ===== Dictionary ===== -}

-- | Check whether a list of words are all in the dictionary.
wordsInDictM :: Dict
            -> [Text]
            -> Evaluator ()
wordsInDictM t ws = mconcat <$> mapM (dictContainsWord t) ws

-- | Check whether a list of words are all in the dictionary
--   outside of the Evaluator monad.
wordsInDict :: Dict
            -> [Text]
            -> Bool
wordsInDict d = all (`Trie.member` d) 

-- | Returns true if the dict contains the given word
dictContainsWord :: Dict -> Text -> Evaluator ()
dictContainsWord d t = Trie.member t d `evalBool` ("Not in dictionary: " <> T.pack (show t)) 

-- | Returns true if the dict contains the given prefix
dictContainsPrefix :: Dict -> Text -> Bool 
dictContainsPrefix d t = not $ Trie.null $ Trie.submap t d 

-- Reads in a dictionary of Scrabble words from the given file.
readDictionary :: FilePath -> IO Dict
readDictionary dict = do
  ls <- lines <$> readFile dict
  pure (Trie.fromList [(T.pack (map toUpper l), ()) | l <- ls])

{- ===== English Dictionary ===== -}

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt" -- "./dict/english2.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionary :: IO Dict
englishDictionary = readDictionary englishDictionaryPath

-- | Read the English dictionary (performing the IO action)
--unsafeReadEnglishDictionary :: Dict
--unsafeReadEnglishDictionary = unsafePerformIO englishDictionary

