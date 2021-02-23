module Scrabble.Dict ( Letter(..)
                     , letterFromChar
                     , toChar)
  where

import Data.Char        (toUpper)
import Data.List        (delete, inits)
import Data.Map         (Map)
import Data.Set         (Set)
import Data.Tuple       (swap)
import Prelude hiding   (Word)
import System.IO.Unsafe

import qualified Data.Maybe as Maybe
import qualified Data.Map   as Map
import qualified Data.Set   as Set

{- ===== Letters ===== -}

data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Enum, Eq, Ord)

instance Show Letter where
  show l = [toChar l]

-- | Convert a Char to a Letter, if the Char is a valid Letter (A-Z).
letterFromChar :: Char -> Maybe Letter
letterFromChar c = Map.lookup c charToLetterMap

-- | Convert a Letter back into a Char. Always valid.
toChar :: Letter -> Char
toChar l = Maybe.fromJust $ Map.lookup l letterToCharMap

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

-- private value.
charToLetterMap :: Map Char Letter
charToLetterMap = Map.fromList (swap <$> letterToCharList)

{- ===== Words ===== -}

type Word = [Letter]

wordToString :: Word -> String
wordToString = fmap toChar

wordFromString :: String -> Maybe Word
wordFromString s = sequence $ letterFromChar <$> s

class HasLetter a where
  letter :: a -> Letter

{- ===== Dictionary ===== -}

data Dict = Dict {
   dictWords    :: Set Word -- ^ All the words in the dictionary
 , dictPrefixes :: Set Word -- ^ The prefixes of all the words in the dictionary.
} deriving Eq

instance Show Dict where
  show d = concat ["(Dict ", nrWords, ", ", nrPrefixes, ")"] where
    nrWords    = "words: "    ++ show (length $ dictWords d)
    nrPrefixes = "prefixes: " ++ show (length $ dictPrefixes d)

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
search' search pred dict prefix rest = match ++ recur prefix rest where
  match = if pred dict prefix then [prefix] else []
  recur prefix _    | not $ dictContainsPrefix dict prefix = []
  recur prefix rest = do l <- rest; search (prefix ++ [l]) (delete l rest)

-- Delete all elements in the first list from the second list.
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll []     hand = hand
deleteAll (x:xs) hand = deleteAll xs (delete x hand)
