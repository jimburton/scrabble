module Scrabble.Dict.Search
  ( findWords
  , findPrefixes
  , wordPlays )
  where

import qualified Data.Set as Set
import Data.Set             ( Set )
import Data.List            ( delete )
import Prelude hiding       ( Word )
import Scrabble.Dict.Dict
  ( Dict
  , dictContainsWord
  , dictContainsPrefix)
import Scrabble.Dict.Letter ( Letter )
import Scrabble.Dict.Word   ( Word )

{- ===== Dictionary Search ===== -}

-- | Find all the words in the dictionary that can be made with the given letters.
findWords :: Dict     -- ^ The dictionary to search
          -> [Letter] -- ^ The letters to build the words from.
          -> Set Word
findWords d = Set.fromList . f [] where f = search' f dictContainsWord d

-- | Find all the prefixes in the dictionary that can be made with the given letters.
findPrefixes :: Dict    -- ^ The dictionary to search
            -> [Letter] -- ^ The letters to build the words from.
            -> Set Word
findPrefixes d = Set.fromList . ([]:) . f [] where f = search' f dictContainsPrefix d

-- | Find all the words that can be made with the letters on the board
--   Returned words are made up of PREFIX + L + SUFFIX, where
--   PREFIX and SUFFIX come from letters in the hand
--   and L is a letter on the board.
wordPlays :: Dict    -- ^ Dictionary to search
         -> [Letter] -- ^ Letters in hand
         -> [Letter] -- ^ Letters on board
         -> Set Word
wordPlays dict hand board = Set.fromList $ do
  prefix <- Set.toList $ findPrefixes dict hand
  letter <- board
  addSuffixes (prefix++[letter]) (deleteAll prefix hand) where
    addSuffixes = search' addSuffixes dictContainsWord dict

type SearchFunc = [Letter] -> [Letter] -> [Word]
type SearchPred = Dict -> Word -> Bool

-- generic recursive dictionary search function.
search' :: SearchFunc -- ^ Function to continue the search
        -> SearchPred -- ^ Predicate that indicates if word matches
        -> Dict       -- ^ The dictionary to search
        -> [Letter]   -- ^ Prefix that all search results must start with
        -> [Letter]   -- ^ The letters to append to the prefix to find matches
        -> [Word]     -- ^ List of words matching
search' search p dict prefix rest = match ++ recur prefix rest where
  match = [prefix | p dict prefix]
  recur prefix' _    | not $ dictContainsPrefix dict prefix' = []
  recur prefix' rest' = do l <- rest'; search (prefix' ++ [l]) (delete l rest')

-- Delete all elements in the first list from the second list.
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll xs hand = foldl (flip delete) hand xs

