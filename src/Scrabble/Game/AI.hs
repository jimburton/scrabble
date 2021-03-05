{-# LANGUAGE TupleSections #-}
module Scrabble.Game.AI ( newGame1P
                        , moveAI )
  where
import Prelude hiding ( Word )
import qualified Data.Trie.Text as Trie
import qualified Data.Map as Map
import Data.List ( maximumBy )
import Data.Functor ( (<&>) )
import System.Random ( StdGen )
import Control.Monad ( msum )
import Scrabble.Evaluator 
  ( Evaluator(..) )
import Scrabble.Types
  ( DictTrie
  , Game(..)
  , Player(..)
  , FreedomDir(..)
  , Rack
  , WordPut
  , Validator
  , Pos
  , Playable
  , Dir(..)
  , Word
  , Turn(..)
  , Letter(Blank) )
import Scrabble.Board.Board
  ( mkWP
  , updateBoard
  , newBoard
  , additionalWords
  , scoreWords )
import Scrabble.Board.Bag
  ( newBag
  , fillRack )
import Scrabble.Lang.Letter
  ( toText )
import Scrabble.Lang.Word (
  wordToString )
import Scrabble.Game.Game
  ( pass )
import Scrabble.Game.Internal
  ( toggleTurn
  , getPlayer
  , updatePlayables
  , updatePlayer
  , setScore )
import Scrabble.Lang.Search
  ( findPrefixesT )

-- =========== AI functions ============ --

-- | Start a new game against the computer.
newGame1P :: String   -- ^ Name of Player
          -> StdGen   -- ^ The random generator
          -> DictTrie -- ^ The dictionary
          -> Game
newGame1P pName theGen d = 
  let Ev (Right (rack1, bag1, gen')) = fillRack [] newBag theGen
      p1 = Player { name  = pName
                  , rack  = rack1
                  , score = 0
                  , isAI = False }
      Ev (Right (rack2, bag2, gen'')) = fillRack [] bag1 gen'
      p2 = Player { name  = "Haskell"
                  , rack  = rack2
                  , score = 0
                  , isAI  = True }
      g  = Game { board     = newBoard
                , bag       = bag2
                , player1   = p1
                , player2   = p2
                , turn      = P1
                , gen       = gen''
                , firstMove = True
                , dict      = d
                , gameOver  = False
                , playable  = Map.empty
                , lastMovePass = False } in
    g

-- | Play a word onto a board as the AI player, Returns the new game and the score of this move.
--   The word is validated by the Validator.
--   Sets the new board, updates the current player's score, refills their rack with letters, then
--   toggles the current turn. Returns the updated game and the score.
moveAI :: Validator -- ^ Function to validate the word against the board.
       -> Game      -- ^ The game.
       -> Evaluator (Game, Int)
moveAI v g = do
  let r  = rack (getPlayer g)
      mw = findWord (dict g) (filter (/=Blank) r) (playable g)
  case mw of
    Nothing -> pass g >>= \g' -> pure (g',0)
    Just w  -> do
      let aw = additionalWords (board g) w
      v (w:aw) g >> scoreWords g w aw >>=
        \i -> setScore g { firstMove = False } i >>= updatePlayer w >>=
        updatePlayables w >>= updateBoard w >>= toggleTurn <&> (,i)

-- | Pick a word for the AI to play. 
findWord :: DictTrie -- ^ The dictionary.
         -> Rack     -- ^ The rack.
         -> Playable -- ^ The playable positions.
         -> Maybe WordPut 
findWord d r p = msum $ Map.mapWithKey findWord' p
  where findWord' k (l,ps) =
          msum $ map (\(fd,i) ->
                         case fd of
                           UpD    -> findPrefixOfSize d k l r (fd,i)
                           DownD  -> findSuffixOfSize d k l r (fd,i)
                           LeftD  -> findPrefixOfSize d k l r (fd,i)
                           RightD -> findSuffixOfSize d k l r (fd,i)) ps

-- | Find a word of at least a certain size that ends with a certain letter.
findPrefixOfSize :: DictTrie         -- ^ The dictionary.
                 -> Pos              -- ^ The end point of the word.
                 -> Letter           -- ^ The letter on the board that this word will connect to.
                 -> Rack             -- ^ The letters from the player's hand to make up the word
                 -> (FreedomDir,Int) -- ^ The direction and max length of the word.
                 -> Maybe WordPut
findPrefixOfSize d p l =
  findWordOfSize (filter ((==l) . last) . findPrefixesT d) p l

-- | Find a word of at least a certain size that begins with a certain letter.
findSuffixOfSize :: DictTrie         -- ^ The dictionary.
                 -> Pos              -- ^ The starting point of the word.
                 -> Letter           -- ^ The letter on the board that this word will connect to.
                 -> Rack             -- ^ The letters from the player's hand to make up the word
                 -> (FreedomDir,Int) -- ^ The direction and max length of the word.
                 -> Maybe WordPut
findSuffixOfSize d k l =
  findWordOfSize (findPrefixesT (Trie.submap (toText l) d)) k l

-- | Get the longest sublist in a list of lists. Not safe (list must have something in it).
longest :: [[a]] -> [a]
longest = maximumBy (\x y -> length x `compare` length y)

type WordFinder = Word -> [Word] 

-- | Find a word of a certain size.
--   TODO max 6 letter words at the moment. Too slow for seven letter words...
findWordOfSize :: WordFinder       -- ^ Function that will query the dictionary.
               -> Pos              -- ^ The start or end point of the word.
               -> Letter           -- ^ The letter on the board that this word will connect to.
               -> Rack             -- ^ The letters from the player's hand to make up the word.
               -> (FreedomDir,Int) -- ^ The direction and max length of the word.
               -> Maybe WordPut
findWordOfSize wf k l r (fd,i) =
  let r' = l : take 5 (filter (/=Blank) r)
      ws = filter ((<=i) . length) $ wf r' in
    if null ws
    then Nothing
    else let w = longest ws
             len = length w - 1
             dir = if fd == UpD || fd == DownD then VT else HZ
             pos = case fd of
               UpD    -> (fst k-len,snd k)
               DownD  -> k
               LeftD  -> (fst k,snd k-len)
               RightD -> k in
      Just $ mkWP (wordToString w) pos dir []
