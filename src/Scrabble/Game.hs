{-# LANGUAGE TupleSections #-}
module Scrabble.Game
  ( Game(..)
  , Turn(..)
  , newGame
  , newGame1P
  , newBoard
  , newBag
  , getPlayer
  , move
  , moveAI
  , valGameRules
  , valGameRulesAndDict
  , swap
  , pass
  , updateBoard )
  where

import Debug.Trace
import System.Random
import Prelude hiding ( Word
                      , words )
import Data.Functor ( (<&>) )
import qualified Data.Map as Map
import Scrabble.Types
  ( Game(..)
  , Turn(..)
  , WordPut
  , Player(..)
  , DictTrie
  , Player
  , Word
  , Rack
  , Board
  , Freedom(..)
  , Dir(..))
import Scrabble.Board.Board
  ( scoreWord
  , validateRack
  , validateMove
  , newBoard
  , empty
  , additionalWords
  , newTilesInMove
  , updateSquare
  , replace
  , freedom
  , freedomsFromWord
  , newTiles
  , getDirection ) 
import Scrabble.Board.Bag
  ( newBag
  , fillRack
  , takeFromRack )
import Scrabble.Evaluator
  ( Evaluator(..) )
import Scrabble.Dict.Dict
  ( wordsInDictT )
import Scrabble.Dict.Word
  ( wordToText ) 

-- ============= Functions for playing the game =============== --


-- | Start a new game.
newGame :: String   -- ^ Name of Player 1
        -> String   -- ^ Name of Player 2
        -> StdGen   -- ^ The random generator
        -> DictTrie -- ^ The dictionary
        -> Game
newGame p1Name p2Name theGen d = 
  let Ev (Right (rack1, bag1, gen')) = fillRack [] newBag theGen
      p1 = Player { name  = p1Name
                  , rack  = rack1
                  , score = 0
                  , isAI = False }
      Ev (Right (rack2, bag2, gen'')) = fillRack [] bag1 gen'
      p2 = Player { name  = p2Name
                  , rack  = rack2
                  , score = 0
                  , isAI  = False }
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

-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated by the Validator.
--   Sets the new board, updates the current player's score, refills their rack with letters, then
--   toggles the current turn. Returns the updated game and the score.
move :: Validator -- ^ Function to validate the word against the board.
     -> Game      -- ^ The game.
     -> WordPut   -- ^ The word to play
     -> [Int]     -- ^ The list positions which were blanks
     -> Evaluator (Game, Int)
move v g w is = do
  let b   = board g
      aw  = additionalWords b w 
  setBlanks w is g >>= v (w:aw) >> scoreWords g w aw >>=
    \i -> setScore g { firstMove = False, lastMovePass = False } i >>= updatePlayer w >>=
    updatePlayables w >>= updateBoard w >>= toggleTurn <&> (,i)

-- | Play a word onto a board as the AI player, Returns the new game and the score of this move.
--   The word is validated by the Validator.
--   Sets the new board, updates the current player's score, refills their rack with letters, then
--   toggles the current turn. Returns the updated game and the score.
moveAI :: Validator -- ^ Function to validate the word against the board.
       -> Game      -- ^ The game.
       -> Evaluator (Game, Int)
moveAI v g = do
  let r  = rack (getPlayer g)
      w  = findWord (dict g) (board g) r
      aw = additionalWords (board g) w
  v (w:aw) g >> scoreWords g w aw >>=
    \i -> setScore g { firstMove = False } i >>= updatePlayer w >>=
    updatePlayables w >>= updateBoard w >>= toggleTurn <&> (,i)

-- ======== AI ========= --

findWord :: DictTrie -> Board -> Rack -> WordPut
findWord d b r = undefined

setBlanks :: WordPut -> [Int] -> Game -> Evaluator Game
setBlanks w bs g = pure (getPlayer g)
                   >>= \p -> pure (p { rack = setBlanks' bs (rack p)})
                   >>= setPlayer g 
  where setBlanks' :: [Int] -> Rack -> Rack
        setBlanks' [] r     = r
        setBlanks' (i:is) r = setBlanks' is (replace r i (fst (snd (w !! i)))) 

-- | Update the score of the current player in the game.
setScore :: Game -- ^ The game to be updated
          -> Int  -- ^ The new score of the current player
          -> Evaluator Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                      pure g { player1 = (player1 g) {score = s' + s} }
               else let s' = score (player2 g) in
                      pure g { player2 = (player2 g) {score = s' + s} }

-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Evaluator Game
updateBoard w g = pure g { board = foldl updateSquare (board g) w}

-- | Update the list of playable positions on the board based on the placement
--   of this word like so:
--   + a horizontal word may add some playable positions to the board in the N and S directions
--   + a vertical word may add some playable positions to the board in the E and W directions
--   + a horizontal word may reduce the playability of positions in the same columns
--   + a vertical word may reduce the playability of positions in the same rows
--
-- TODO handle updating old playables
updatePlayables :: WordPut -> Game -> Evaluator Game
updatePlayables w g = do let ps = (trace ("Playable: "++show (playable g))) $ playable g
                             b  = board g
                             nt = newTiles b w 
                             d  = getDirection w
                             fs = freedomsFromWord nt b
                             f (u,p) = if d == HZ
                                       then Freedom { north = u
                                                    , east  = 0
                                                    , south = p
                                                    , west  = 0 }
                                       else Freedom { north = 0
                                                    , east  = p
                                                    , south = 0
                                                    , west  = u }
                             nps = foldl (\acc (p,l,(n,s)) -> Map.insert p (l,f (n,s)) acc) ps fs
                         pure (g { playable = nps })

-- | Update the rack of the current player
updatePlayer :: WordPut -> Game -> Evaluator Game
updatePlayer w g = do
  let p      = getPlayer g
      r      = rack p
      theBag = bag g
      theGen = gen g
  takeFromRack r (map (fst . snd) (filter (\(pos,_) -> empty (board g) pos) w))
    >>= \r' -> fillRack r' theBag theGen
    >>= \(r'', theBag', theGen') -> setPlayer g (p { rack = r'' })
    >>= \g' -> pure g' { bag = theBag', gen = theGen'}

swap :: Word -> Game -> Evaluator Game
swap ls g = do
  let p      = getPlayer g
      r      = rack p
      theBag = bag g
      theGen = gen g
  takeFromRack r ls >>= \r' -> fillRack r' theBag theGen
    >>= \(r'', theBag', theGen') -> setPlayer g (p { rack = r'' })
    >>= toggleTurn >>= \g' -> pure g' { bag = theBag', gen = theGen' }

pass :: Game -> Evaluator Game
pass g = if lastMovePass g
         then endGame g
         else toggleTurn g { lastMovePass = True }

endGame :: Game -> Evaluator Game
endGame g = pure g { gameOver = True }

-- | Calculate the score of playing a word onto the board, including
--   bonuses and other words that may be created.
scoreWords :: Game -> WordPut -> [WordPut] -> Evaluator Int
scoreWords g w aw = do
  let b = board g
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      fpb = if newTilesInMove b w == 7 then 50 else 0
  pure $ sum $ map (scoreWord fpb) sws
  
-- | Update the current player in the game. 
setPlayer :: Game -> Player -> Evaluator Game
setPlayer g p = if turn g == P1
                then pure g { player1 = p }
                else pure g { player2 = p }

-- | Get the current player in the game.
getPlayer :: Game -> Player
getPlayer g = if turn g == P1 then player1 g else player2 g

-- | Toggle the turn in the game (between P1 and P2)
toggleTurn :: Game -- ^ The game in which to toggle the turn
           -> Evaluator Game
toggleTurn g = pure g { turn = if turn g == P1 then P2 else P1 }

-- | Validator is the type of functions that validate words to be played
type Validator = [WordPut] -> Game -> Evaluator Bool

-- | Validate a set of words against the rules of the game and the dictionary. 
valGameRulesAndDict :: Validator
valGameRulesAndDict ws g = do
  let d  = dict g
      ts = map (wordToText . map (fst .snd)) ws
  valGameRules ws g >> wordsInDictT d ts 

-- | Validate a set of words against the rack (are all tiles in the current
--   player's rack or on the board?) and that the move is in the rules of the
--   game (each word except the first one connects to an existing word, whereas the
--   first one to be played must touch the centre square)
valGameRules :: Validator
valGameRules ws g = do
  let b  = board g
      p  = getPlayer g
      w  = head ws
      fm = firstMove g
  validateRack b (rack p) w >> validateMove b p w fm


  
