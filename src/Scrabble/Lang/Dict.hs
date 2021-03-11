module Scrabble.Lang.Dict
  ( letterFromChar
  , toChar
  , wordsInDictT
  , wordsInDict
  , englishDictionaryT
  , dictContainsWordT
  , dictContainsPrefixT )
  where

import Data.Char        ( toUpper )
import Prelude hiding   ( Word )
import Data.Text        ( Text
                        , pack)
import Data.Monoid ( mconcat )
import qualified Data.Trie.Text as Trie

import Scrabble.Lang.Letter
  ( letterFromChar
  , toChar )
import Scrabble.Evaluator
  ( Evaluator(..)
  , evalBool )
import Scrabble.Types
  ( DictTrie )

{- ===== Dictionary ===== -}

-- | Check whether a list of words are all in the dictionary.
wordsInDictT :: DictTrie
            -> [Text]
            -> Evaluator ()
wordsInDictT t ws = mconcat <$> mapM (dictContainsWordT t) ws

-- | Check whether a list of words are all in the dictionary
--   outside of the Evaluator monad.
wordsInDict :: DictTrie
            -> [Text]
            -> Bool
wordsInDict d = all (`Trie.member` d) 

-- | Returns true if the dict contains the given word
dictContainsWordT :: DictTrie -> Text -> Evaluator ()
dictContainsWordT d t = Trie.member t d `evalBool` ("Not in dictionary: " ++ show t) 

-- | Returns true if the dict contains the given prefix
dictContainsPrefixT :: DictTrie -> Text -> Bool 
dictContainsPrefixT d t = not $ Trie.null $ Trie.submap t d 

-- Reads in a dictionary of Scrabble words from the given file.
readDictionaryT :: FilePath -> IO DictTrie
readDictionaryT dict = do
  ls <- lines <$> readFile dict
  return (Trie.fromList [(pack (map toUpper l), True) | l <- ls])

{- ===== English Dictionary ===== -}

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt" -- "./dict/english2.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionaryT :: IO DictTrie
englishDictionaryT = readDictionaryT englishDictionaryPath

-- | Read the English dictionary (performing the IO action)
--unsafeReadEnglishDictionary :: Dict
--unsafeReadEnglishDictionary = unsafePerformIO englishDictionary
