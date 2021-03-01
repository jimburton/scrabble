module Scrabble.Game
  ( Game(..)
  , Turn(..)
  , newGame
  , newBoard
  , newBag
  , takeFromRack
  , fillRack
  , getPlayer
  , setPlayer
  , toggleTurn
  , moveM
  , moveM')
  where

import System.Random
import Prelude hiding ( words )
import Scrabble.Types
  ( Game(..)
  , Turn(..)
  , WordPut
  , Player(..)
  , DictTrie )
import Scrabble.Board.Board
  ( scoreWord
  , validateRackM
  , validateMoveM 
  , newBoard
  , updateBoard
  , empty
  , additionalWords
  , newTilesInMove )
import Scrabble.Board.Bag
  ( newBag
  , fillRack
  , takeFromRack )
import Scrabble.Evaluator
  ( Evaluator(..) )
import Scrabble.Dict.Dict
  ( wordsInDictT )
import Scrabble.Dict.Search
  ( wordToText ) 

-- ============= Functions for playing the game =============== --


-- | Start a new game.
newGame :: String -- ^ Name of Player 1
        -> String -- ^ Name of Player 2
        -> StdGen -- ^ The random generator
        -> Game
newGame p1Name p2Name theGen = 
  let (rack1, bag1, gen') = fillRack [] newBag theGen
      p1 = Player { name = p1Name
                  , rack = rack1
                  , score = 0 }
      (rack2, bag2, gen'') = fillRack [] bag1 gen'
      p2 = Player { name = p2Name
                  , rack = rack2
                  , score = 0 }
      g  = Game { board = newBoard
                , bag = bag2
                , player1 = p1
                , player2 = p2
                , turn = P1
                , gen = gen''
                , firstMove = True } in
    g

-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated as being in the dictionary.
moveM :: DictTrie    -- ^ The dictionary
     -> Game    -- ^ The game
     -> WordPut -- ^ The word to play
     -> Evaluator (Game, Int)
moveM d g w  = do
  let b   = board g
      fm  = firstMove g
      p   = getPlayer g
      aw  = additionalWords b w 
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      waw = map (wordToText . map snd) (w:aw)
      fpb = if newTilesInMove b w == 7 then 50 else 0
      sc  = sum $ map (scoreWord fpb) sws 
  validateRackM b (rack p) w >> validateMoveM b p w fm >> wordsInDictT d waw >> do 
    let g' = setScore g sc 
    pure (g' {board = updateBoard b w, firstMove = False}, sc)

moveM' :: DictTrie    -- ^ The dictionary
     -> Game    -- ^ The game
     -> WordPut -- ^ The word to play
     -> Evaluator (Game, Int)
-- | Move with no dictionary check, for convenience in testing
moveM' _ g w  = do
  let b   = board g
      fm  = firstMove g
      p   = getPlayer g
      aw  = additionalWords b w 
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      fpb = if newTilesInMove b w == 7 then 50 else 0
      sc  = sum $ map (scoreWord fpb) sws 
  validateRackM b (rack p) w >> validateMoveM b p w fm >> do 
    let g' = setScore g sc 
    pure (g' {board = updateBoard b w, firstMove = False}, sc)

-- | Update the current player in the game. 
setPlayer :: Game -> Player -> Game
setPlayer g p = if turn g == P1
                then g { player1 = p }
                else g { player2 = p }

-- | Toggle the turn in the game (between P1 and P2)
toggleTurn :: Game -- ^ The game in which to toggle the turn
           -> Game
toggleTurn g = g { turn = if turn g == P1 then P2 else P1 }

-- | Update the score of the current player in the game.
setScore :: Game -- ^ The game to be updated
         -> Int  -- ^ The new score of the current player
         -> Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                    g { player1 = (player1 g) {score = s' + s} }
               else let s' = score (player2 g) in
                    g { player2 = (player2 g) {score = s' + s} }

-- | Get the current player in the game.
getPlayer :: Game -> Player
getPlayer g = if turn g == P1 then player1 g else player2 g
  
