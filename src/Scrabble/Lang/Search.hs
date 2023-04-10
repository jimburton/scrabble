{-|
Module      : Scrabble.Lang.Search
Description : Functions relating to searching the dictionary for the Scrabble game.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions relating to searching the dictionary for the Scrabble game.
-}
module Scrabble.Lang.Search
  ( findWords
  , makeWords
  , findPrefixes
  , findSuffixes
  , wordsInDictM
  , wordsInDict
  , dictContainsWord
  , dictContainsPrefix )
  where

import Control.Lens ((^.))
import Data.List (nub, permutations)
import Prelude hiding (Word)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (filterM)
import qualified Data.Trie as Trie
import Scrabble.Types
  ( Dict
  , Word
  , Letter
  , Word
  , Game
  , dict
  , Evaluator)
import Scrabble.Evaluator (evalBool)
import Scrabble.Lang.Word
  ( textToWord
  , wordToText ) 

-- * Dictionary Search

-- | Find all the words in the given list of words that are in the dictionary.
findWords :: Game   -- ^ The dictionary to search
          -> [Text] -- ^ The letters to build the words from.
          -> [Word] -- ^ The words from the dictionary.
findWords g ws = map textToWord $ filter ((`Trie.member` (g ^. dict)) . encodeUtf8) ws

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
makeWords :: Game   -- ^ The game.
          -> Word   -- ^ The letters to build the words from.
          -> [Word] -- ^ The words from the dictionary.
makeWords g ls = findWords g (map wordToText (perms ls))

-- | Find all words in the dictionary that end with the given letter.
findPrefixes :: Game    -- ^ The game.
             -> Letter -- ^ The suffix.
             -> Word   -- ^ The letters to build the words from.
             -> [Word] -- ^ The prefixes.
findPrefixes g l ls = findWords g (map (wordToText . (++[l])) (perms ls))

-- | Find all words in the dictionary that begin with the given letter.
findSuffixes :: Game    -- ^ The game.
             -> Letter -- ^ The suffix.
             -> Word   -- ^ The letters to build the words from.
             -> [Word] -- ^ The suffixes.
findSuffixes g l ls = findWords g (map (wordToText . (l:)) (perms ls))

-- | Find all the words that can be made with the letters on the board
--   Returned words are made up of PREFIX + L + SUFFIX, where
--   PREFIX + SUFFIX is a partition of the letters in the hand
--   and L is a letter on the board.
--   TODO: get this working! 
{-wordPlaysT :: Game     -- ^ The game.
           -> [Letter] -- ^ Letters in hand
           -> [Letter] -- ^ Letters on board
           -> [Word]   -- ^ The list of words. 
wordPlaysT g ls b = undefined -- findWords g ((map parts ls) (perms ls))
-}

-- | Check whether a list of words are all in the dictionary.
wordsInDictM :: Dict        -- ^ The dictionary.
            -> [Text]       -- ^ The list of words.
            -> Evaluator () 
wordsInDictM t ws = mconcat <$> mapM (dictContainsWord t) ws

-- | Check whether a list of words are all in the dictionary
--   outside of the Evaluator monad.
wordsInDict :: Dict   -- ^ The dictionary.
            -> [Text] -- ^ The list of words.
            -> Bool
wordsInDict d = all ((`Trie.member` d) . encodeUtf8) 

-- | Returns true if the dict contains the given word
dictContainsWord :: Dict -> Text -> Evaluator ()
dictContainsWord d t = Trie.member (encodeUtf8 t) d `evalBool` ("Not in dictionary: " <> t) 

-- | Returns true if the dict contains the given prefix
dictContainsPrefix :: Dict -> Text -> Bool 
dictContainsPrefix d t = not $ Trie.null $ Trie.submap (encodeUtf8 t) d
