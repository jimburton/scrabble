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
module Scrabble.Dict
  ( englishDictionary
  , letterToChar
  , scoreLetter )
  where

import Prelude hiding (Word)
import Codec.Binary.UTF8.String (encode)
import qualified Data.ByteString as B
import Data.Char (toUpper)
import qualified Data.Trie as Trie
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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
  pure (Trie.fromList [(B.pack $ encode (map toUpper l), ()) | l <- ls])

-- ===== English Dictionary ===== --

-- English dictionary file path
englishDictionaryPath :: FilePath
englishDictionaryPath = "./dict/en.txt" -- "./dict/english2.txt"

-- | Reads in the (English) dictionary of Scrabble words.
englishDictionary :: IO Dict
englishDictionary = readDictionary englishDictionaryPath

-- * Letters

-- | Convert a @Char@ to a @Letter@, if the @Char@ is a valid @Letter@ (A-Blank).
letterFromChar :: Char -> Maybe Letter
letterFromChar c = Map.lookup c charToLetterMap

-- | Convert a @Letter@ into a @Char@. Always valid.
letterToChar :: Letter -> Char
letterToChar l = fromJust $ Map.lookup l letterToCharMap

-- | Convert a @Letter@ to @Text@.
letterToText :: Letter -> Text
letterToText = T.pack . show

-- private value.
letterToCharList :: [(Letter,Char)]
letterToCharList = [
  (A, 'A'), (B, 'B'), (C, 'C'), (D, 'D'), (E, 'E'), (F, 'F'), (G, 'G'),
  (H, 'H'), (I, 'I'), (J, 'J'), (K, 'K'), (L, 'L'), (M, 'M'), (N, 'N'),
  (O, 'O'), (P, 'P'), (Q, 'Q'), (R, 'R'), (S, 'S'), (T, 'T'), (U, 'U'),
  (V, 'V'), (W, 'W'), (X, 'X'), (Y, 'Y'), (Z, 'Z'), (Blank, '_') ]

-- private value.
letterToCharMap :: Map Letter Char
letterToCharMap = Map.fromList letterToCharList

-- | Mapping chars to letters.
charToLetterMap :: Map Char Letter
charToLetterMap = Map.fromList (swap <$> letterToCharList)

-- lookup table for the score of a letter
letterToScoreList :: [(Letter,Int)]
letterToScoreList = [
  (A, 1), (B, 3), (C, 3), (D, 2), (E, 1), (F, 4), (G, 2),
  (H, 4), (I, 1), (J, 8), (K, 5), (L, 1), (M, 3), (N, 1),
  (O, 1), (P, 3), (Q, 10), (R, 1), (S, 1), (T, 1), (U, 1),
  (V, 4), (W, 4), (X, 8), (Y, 4), (Z, 10), (Blank, 0) ]

-- map to find the score of a letter
letterToScoreMap :: Map Letter Int
letterToScoreMap = Map.fromList letterToScoreList

-- | Find the score of a letter.
scoreLetter :: Letter -> Int
scoreLetter = fromJust . flip Map.lookup letterToScoreMap

-- * Words

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

-- * Searching the dictionary

findWords :: Dict   -- ^ The dictionary to search
          -> [Text] -- ^ The letters to build the words from.
          -> [Word] -- ^ The words from the dictionary.
findWords d ws = map textToWord $ filter ((`Trie.member` d) . encodeUtf8) ws

-- Ordered list of values in a bounded enumeration.
domain :: (Bounded a, Enum a) => [a]
domain = [minBound..maxBound] 


-- Generate a power set.  The algorithm used here is from
--   <http://evan-tech.livejournal.com/220036.html>.
powerSet :: [a] -> [[a]]
powerSet = filterM (const domain)


-- Generate a power set's permutations.
powerSetPermutations :: [a] -> [[a]]
powerSetPermutations = concatMap permutations . powerSet

-- Generate a power set's unique permutations.
uniquePowerSetPermutations :: Eq a => [a] -> [[a]]
uniquePowerSetPermutations = nub . powerSetPermutations

-- Permutations of a list
perms :: Eq a => [a] -> [[a]]
perms = filter ((>1) . length) . uniquePowerSetPermutations 

-- | Find all words in the dictionary that can be made with any combination of
--   the given letters.
makeWords :: Dict   -- ^ The game.
          -> Word   -- ^ The letters to build the words from.
          -> [Word] -- ^ The words from the dictionary.
makeWords d ls = findWords d (map wordToText (perms ls))

-- | Check whether a list of words are all in the dictionary
--   outside of the Evaluator monad.
wordsInDict :: Dict   -- ^ The dictionary.
            -> [Text] -- ^ The list of words.
            -> Bool
wordsInDict d = all ((`Trie.member` d) . encodeUtf8) 

-- | Returns true if the dict contains the given prefix
dictContainsPrefix :: Dict -> Text -> Bool 
dictContainsPrefix d t = not $ Trie.null $ Trie.submap (encodeUtf8 t) d 

