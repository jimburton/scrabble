module Scrabble.Dict.Dict
  ( letterFromChar
  , toChar
  , wordsInDict
  , wordsInDictM
  , englishDictionary
  , dictContainsWord
  , dictContainsPrefix )
  where

import Data.Char        ( toUpper )
import Data.List        ( inits )
import Prelude hiding   ( Word )

import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import Scrabble.Dict.Letter
  ( letterFromChar
  , toChar )
import Scrabble.Dict.Word
  ( wordToString)
import Scrabble.Evaluator
  ( Evaluator(..) )
import Scrabble.Types
  ( Word
  , Dict(..) )

{- ===== Dictionary ===== -}

-- | Check whether a list of words are all in the dictionary.
wordsInDictM :: Dict
            -> [Word]
            -> Evaluator Bool
wordsInDictM d ws = and <$> mapM (dictContainsWordM d) ws

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
dictContainsWordM :: Dict -> Word -> Evaluator Bool
dictContainsWordM d w = if Set.member w (dictWords d)
                        then Ev $ Right True
                        else Ev $ Left ("Not in dictionary: " ++ show w)

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
  wordsAndPrefixes dict' = unzip $ wordWithPrefixes <$> lines dict' where
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
--unsafeReadEnglishDictionary :: Dict
--unsafeReadEnglishDictionary = unsafePerformIO englishDictionary

