{-# LANGUAGE TupleSections #-}
module Scrabble.Game
  ( Game(..)
  , Turn(..)
  , newGame
  , newBoard
  , newBag
  , getPlayer
  , setPlayer
  , toggleTurn
  , moveM
  , moveM'
  , move
  , valWithDict )
  where

import System.Random
import Prelude hiding ( words )
import Data.Functor ( (<&>) )
import Scrabble.Types
  ( Game(..)
  , Turn(..)
  , WordPut
  , Player(..)
  , DictTrie
  , Board
  , Player
  , Rack
  , Bag )
import Scrabble.Board.Board
  ( scoreWord
  , validateRackM
  , validateMoveM 
  , newBoard
  , updateBoard
  , empty
  , additionalWords
  , newTilesInMove
  , updateSquare )
import Scrabble.Board.Bag
  ( newBag
  , fillRack
  , takeFromRack
  , fillRackM
  , takeFromRackM )
import Scrabble.Evaluator
  ( Evaluator(..) )
import Scrabble.Dict.Dict
  ( wordsInDictT )
import Scrabble.Dict.Word
  ( wordToText ) 

-- ============= Functions for playing the game =============== --


-- | Start a new game.
newGame :: String -- ^ Name of Player 1
        -> String -- ^ Name of Player 2
        -> StdGen -- ^ The random generator
        -> DictTrie -- ^ The dictionary
        -> Game
newGame p1Name p2Name theGen d = 
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
                , firstMove = True
                , dict = d } in
    g

-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated as being in the dictionary.

-- | Updates the game after a move is played onto the board. Sets the new board,
--   updates the current player's score, refills their rack with letters, then
--   toggles the current turn.
move :: Validator
     -> Game
     -> WordPut -- ^ The word to play
     -> Evaluator (Game, Int)
move v g w  = do
  let b   = board g
      aw  = additionalWords b w 
  v g (w:aw) >> scoreWords g w aw >>=
    \i -> setScoreM g i >>= updatePlayer w >>=
    updateBoardM w >>= toggleTurnM <&> (,i)

setScoreM :: Game -> Int -> Evaluator Game
setScoreM g i = pure (setScore g i)

updateBoardM :: WordPut -> Game -> Evaluator Game
updateBoardM w g = pure g { board = foldl updateSquare (board g) w}


-- | Update the rack of the current player
updatePlayer :: WordPut -> Game -> Evaluator Game
updatePlayer w g = do
  let p      = getPlayer g
      r      = rack p
      theBag = bag g
      theGen = gen g
  takeFromRackM r w >>= \r' -> fillRackM r' theBag theGen
    >>= \(r'', theBag', theGen') -> do
    let p' = p { rack = r''}
        g' = setPlayer g p'
    pure g' { bag = theBag', gen = theGen'}

scoreWords :: Game -> WordPut -> [WordPut] -> Evaluator Int
scoreWords g w aw = do
  let b = board g
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      fpb = if newTilesInMove b w == 7 then 50 else 0
  pure $ sum $ map (scoreWord fpb) sws
  
moveM :: Game    -- ^ The game
      -> WordPut -- ^ The word to play
      -> Evaluator (Game, Int)
moveM g w  = do
  let d   = dict g
      b   = board g
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

-- | Move with no dictionary check, for convenience in testing
moveM' :: Game    -- ^ The game
       -> WordPut -- ^ The word to play
       -> Evaluator (Game, Int)
moveM' g w  = do
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

-- | Update the current player in the game. 
setPlayerM :: Game -> Player -> Evaluator Game
setPlayerM g p = if turn g == P1
                then pure g { player1 = p }
                else pure g { player2 = p }

-- | Toggle the turn in the game (between P1 and P2)
toggleTurn :: Game -- ^ The game in which to toggle the turn
           -> Game
toggleTurn g = g { turn = if turn g == P1 then P2 else P1 }

-- | Toggle the turn in the game (between P1 and P2)
toggleTurnM :: Game -- ^ The game in which to toggle the turn
           -> Evaluator Game
toggleTurnM g = pure g { turn = if turn g == P1 then P2 else P1 }

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

type Validator = Game -> [WordPut] -> Evaluator Bool

valWithDict :: Validator
valWithDict g ws = do
  let b  = board g
      p  = getPlayer g
      w  = head ws
      fm = firstMove g
      d  = dict g
      ts = map (wordToText . map snd) ws
  validateRackM b (rack p) w >> validateMoveM b p w fm >> wordsInDictT d ts

valNoDict :: Validator
valNoDict g ws = do
  let b  = board g
      p  = getPlayer g
      w  = head ws
      fm = firstMove g
      d  = dict g
  validateRackM b (rack p) w >> validateMoveM b p w fm

  
