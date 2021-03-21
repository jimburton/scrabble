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
  , findPrefixes
  , wordPlaysT
  , wordsInDictM
  , wordsInDict
  , dictContainsWord
  , dictContainsPrefix )
  where

import Data.List
  ( nub
  , permutations )
import Prelude hiding       ( Word )
import Data.Text            ( Text )
import qualified Data.Text as T
import Control.Monad        (filterM)
import qualified Data.Trie.Text as Trie
import Scrabble.Types
  ( Dict
  , Word
  , Letter
  , Word
  , Game(..)
  , Evaluator)
import Scrabble.Evaluator (evalBool)
import Scrabble.Lang.Word
  ( textToWord
  , wordToText ) 

{- ===== Dictionary Search ===== -}

-- | Find all the words in the dictionary that can be made with the given letters.
findWords :: Game -- ^ The dictionary to search
          -> [Text]    -- ^ The letters to build the words from.
          -> [Word]
findWords g ws = map textToWord $ filter (`Trie.member` dict g) ws

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
perms xs = filter ((>1) . length) $ uniquePowerSetPermutations xs

-- | Find all the prefixes in the dictionary that can be made with the given letters.
findPrefixes :: Game    -- ^ The game.
             -> Word   -- ^ The letters to build the words from.
             -> [Word]
findPrefixes g ls = findWords g (map wordToText (perms ls))

-- | Find all the words that can be made with the letters on the board
--   Returned words are made up of PREFIX + L + SUFFIX, where
--   PREFIX + SUFFIX is a partition of the letters in the hand
--   and L is a letter on the board.
--   TODO: get this working! 
wordPlaysT :: Dict    -- ^ Dictionary to search
         -> [Letter] -- ^ Letters in hand
         -> [Letter] -- ^ Letters on board
         -> [Word]
wordPlaysT = undefined -- map (\(i,t) -> i ++ 'L':t) $ zip (inits str) (tails str)

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

