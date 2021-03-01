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
  , takeMoveM )
  where

import System.Random
import Prelude hiding ( words )
import Scrabble.Types
  ( Game(..)
  , Turn(..)
  , WordPut
  , Player(..)
  , Dict )
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
  ( wordsInDictM )

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
                , gen = gen'' } in
    g

-- | Take a move. Checks that this word is in the player's rack then calls the
--   move function. Returns the new game and the score of this move.
takeMoveM :: Dict    -- ^ The dictionary
         -> Game    -- ^ The game
         -> WordPut -- ^ The word to play
         -> Bool    -- ^ Is first move
         -> Evaluator (Game, Int)
takeMoveM d g w fm = validateRackM (board g) ((rack . getPlayer) g) w >> moveM d g w fm

-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated as being in the dictionary.
moveM :: Dict    -- ^ The dictionary
     -> Game    -- ^ The game
     -> WordPut -- ^ The word to play
     -> Bool    -- ^ Is first move
     -> Evaluator (Game, Int)
moveM d g w fm = do
  let b   = board g
      p   = getPlayer g
      aw  = additionalWords b w 
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      waw = map (map snd) (w:aw)
      fpb = if newTilesInMove b w == 7 then 50 else 0
      sc  = sum $ map (scoreWord fpb) sws 
  validateMoveM b p w fm >> wordsInDictM d waw >> do 
    let g' = setScore g sc 
    pure (g' {board = updateBoard b w}, sc)

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
  
