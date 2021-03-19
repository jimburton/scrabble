{-# LANGUAGE TupleSections #-}
module Scrabble.Game.Game
  ( Game(..)
  , Turn(..)
  , newGame
  , newBoard
  , newBag
  , getPlayer
  , move
  , swap
  , pass
  , updateBoard )
  where

import Debug.Trace
import System.Random
import Prelude hiding
  ( Word
  , words )
import Data.Functor ( (<&>) )
import qualified Data.Map as Map
import Data.Text ( Text )
import Scrabble.Game.Internal
  ( getPlayer
  , setPlayer 
  , setBlanks
  , setScore
  , updatePlayables
  , toggleTurn
  , updatePlayer
  , checkEndOfGame
  , endGame
  , endNonPassMove )  
import Scrabble.Types
  ( Game(..)
  , Turn(..)
  , WordPut
  , Player(..)
  , Dict
  , Word
  , Validator )
import Scrabble.Board.Board
  ( scoreWords
  , newBoard
  , additionalWords
  , updateBoard )
import Scrabble.Board.Internal
  ( wordPutToWord )
import Scrabble.Board.Bag
  ( newBag
  , fillRack
  , takeFromRack )
import Scrabble.Evaluator
  ( Evaluator(..) )

-- ============= Functions for playing the game =============== --

-- | Start a new game.
newGame :: Text   -- ^ Name of Player 1
        -> Text   -- ^ Name of Player 2
        -> StdGen   -- ^ The random generator
        -> Dict -- ^ The dictionary
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

-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated by the Validator.
--   Sets the new board, updates the current player's score, refills their rack with letters, then
--   toggles the current turn. Returns the updated game and a pair of the words played (including
--   additional words generated) and the score.
move :: Validator -- ^ Validates the word against the board.
     -> Game      -- ^ The game.
     -> WordPut   -- ^ The word to play
     -> [Int]     -- ^ The list positions which were blanks
     -> Evaluator (Game, ([Word],Int))
move validate g w is = additionalWords g w >>= \aw -> setBlanks w is g
  >>= \g' -> validate (w:aw) g' >> scoreWords g w aw
  >>= \sc -> setScore g' sc 
  >>= updatePlayer w >>= updatePlayables w >>= updateBoard w
  >>= endNonPassMove >>= checkEndOfGame <&> (,(map wordPutToWord (w:aw),sc))

-- | Take a move by swapping tiles.
swap :: Word -- ^ The tiles to swap.
     -> Game -- ^ The game.
     -> Evaluator Game
swap ls g = do
  let p      = getPlayer g
      r      = rack p
      theBag = bag g
      theGen = gen g
  takeFromRack r ls >>= \r' -> fillRack r' theBag theGen
    >>= \(r'', theBag', theGen') -> setPlayer g (p { rack = r'' })
    >>= endNonPassMove >>= checkEndOfGame
    >>= \g' -> pure g' { bag = ls++theBag'
                       , gen = theGen'
                       , lastMovePass = False }

-- | Take a move by passing.
pass :: Game -> Evaluator Game
pass g = if lastMovePass g
         then endGame g
         else toggleTurn g { lastMovePass = True } >>= checkEndOfGame

  
