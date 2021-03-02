module Scrabble.Dict.Search
  ( findWordsT
  , findPrefixesT
  , wordPlaysT )
  where

import Data.List
  ( nub
  , permutations )
import Prelude hiding       ( Word )
import Data.Text            ( Text )
import Control.Monad        (filterM)
import qualified Data.Trie.Text as Trie
import Scrabble.Types
  ( DictTrie
  , Word
  , Letter
  , Word )
import Scrabble.Dict.Word
  ( textToWord
  , wordToText ) 

{- ===== Dictionary Search ===== -}

-- | Find all the words in the dictionary that can be made with the given letters.
findWordsT :: DictTrie -- ^ The dictionary to search
          -> [Text]    -- ^ The letters to build the words from.
          -> [Word]
findWordsT d ws = map textToWord $ filter (`Trie.member` d) ws

-- | Ordered list of values in a bounded enumeration.
domain :: (Bounded a, Enum a) => [a]
domain = [minBound..maxBound]


-- | Generate a power set.  The algorithm used here is from
--   <http://evan-tech.livejournal.com/220036.html>.
powerSet :: [a] -> [[a]]
powerSet = filterM (const domain)


-- | Generate a power set's permutations.
powerSetPermutations :: [a] -> [[a]]
powerSetPermutations = concatMap permutations . powerSet

-- | Generate a power set's unique permutations.
uniquePowerSetPermutations :: Eq a => [a] -> [[a]]
uniquePowerSetPermutations = nub . powerSetPermutations

perms :: Eq a => [a] -> [[a]]
perms xs = filter ((>1) . length) $ uniquePowerSetPermutations xs

-- | Find all the prefixes in the dictionary that can be made with the given letters.
findPrefixesT :: DictTrie    -- ^ The dictionary to search
              -> Word -- ^ The letters to build the words from.
              -> [Word]
findPrefixesT t ls = findWordsT t (map wordToText (perms ls))

-- | Find all the words that can be made with the letters on the board
--   Returned words are made up of PREFIX + L + SUFFIX, where
--   PREFIX and SUFFIX come from letters in the hand
--   and L is a letter on the board.
wordPlaysT :: DictTrie    -- ^ Dictionary to search
         -> [Letter] -- ^ Letters in hand
         -> [Letter] -- ^ Letters on board
         -> [Word]
wordPlaysT = undefined -- map (\(i,t) -> i ++ 'L':t) $ zip (inits str) (tails str) 
