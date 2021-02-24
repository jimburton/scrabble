module Scrabble.Dict.Dict ( Dict
                          , letterFromChar
                          , toChar
                          , wordsInDict
                          , englishDictionary
                          , dictContainsWord
                          , dictContainsPrefix )
  where

import Data.Char        ( toUpper )
import Data.List        ( inits )
import Data.Set         ( Set )
import Prelude hiding   ( Word )
import System.IO.Unsafe

import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import Scrabble.Dict.Letter
  ( letterFromChar
  , toChar )
import Scrabble.Dict.Word
  ( Word 
  , wordToString)

{- ===== Dictionary ===== -}

data Dict = Dict {
   dictWords    :: Set Word -- ^ All the words in the dictionary
 , dictPrefixes :: Set Word -- ^ The prefixes of all the words in the dictionary.
} deriving Eq

instance Show Dict where
  show d = concat ["(Dict ", nrWords, ", ", nrPrefixes, ")"] where
    nrWords    = "words: "    ++ show (length $ dictWords d)
    nrPrefixes = "prefixes: " ++ show (length $ dictPrefixes d)

-- | Check whether a list of words are all in the dictionary.
wordsInDict :: Dict
            -> [Word]
            -> Either String Bool
wordsInDict _ []     = Right True
wordsInDict d (w:ws) = let wStr = wordToString w in
                       if dictContainsWord d w
                       then wordsInDict d ws
                       else Left ("Not in dictionary: "++wStr) 

-- | Returns true if the dict contains the given word
dictContainsWord :: Dict -> Word -> Bool
dictContainsWord = flip Set.member . dictWords

-- | Returns true if the dict contains the given prefix
dictContainsPrefix :: Dict -> Word -> Bool
dictContainsPrefix = flip Set.member . dictPrefixes

-- Reads in a dictionary of Scrabble words from the given file.
readDictionary :: FilePath -> IO Dict
readDictionary dict = mkDict <$> readFile dict where
  mkDict :: String -> Dict
  mkDict = uncurry dictFromLists . wordsAndPrefixes
  wordsAndPrefixes :: String -> ([Word], [[Word]])
  wordsAndPrefixes dict = unzip $ wordWithPrefixes <$> lines dict where
    -- return the word, and all its prefixes
    wordWithPrefixes :: String -> (Word, [Word])
    wordWithPrefixes w =
      -- first turn each char into a Letter, when create result
      let w' = Maybe.fromJust . letterFromChar . toUpper <$> w
      in (w', init $ inits w')
  -- list based constructor for Dict
  dictFromLists :: [Word] -> [[Word]] -> Dict
  dictFromLists wordList prefixesLists =
    Dict (Set.fromList wordList) (Set.fromList $ concat prefixesLists)

{- ===== English Dictionary ===== -}

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionary :: IO Dict
englishDictionary = readDictionary englishDictionaryPath

-- | Read the English dictionary (performing the IO action)
unsafeReadEnglishDictionary :: Dict
unsafeReadEnglishDictionary = unsafePerformIO englishDictionary

