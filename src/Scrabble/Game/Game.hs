{-# LANGUAGE TupleSections #-}
{-|
Module      : Scrabble.Game.Game
Description : Functions for playing a game of scrabble (for a human player).
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for playing a game of scrabble (for a human player).
-}
module Scrabble.Game.Game
  ( newGame
  , newBoard
  , newBag
  , getPlayer
  , move
  , swap
  , pass )
  where

import System.Random ( StdGen )
import Prelude hiding
  ( Word
  , words )
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Text (Text)
import Lens.Simple ((.~),(^.),(&))
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
  , lastMovePass, gen, bag, turn
  , Turn(..)
  , WordPut
  , Player(..)
  , rack
  , Dict
  , Word
  , Validator
  , MoveResult(..) )
import Scrabble.Board.Board
  ( scoreWords
  , newBoard
  , additionalWords
  , updateBoard
  , wordPutToWord )
import Scrabble.Board.Bag
  ( newBag
  , fillRack
  , takeFromRack )
import Scrabble.Evaluator (Evaluator(..))

-- ============= Functions for playing the game =============== --

-- | Start a new game.
newGame :: Text   -- ^ Name of Player 1
        -> Text   -- ^ Name of Player 2
        -> StdGen   -- ^ The random generator
        -> Dict -- ^ The dictionary
        -> Game
newGame p1Name p2Name theGen d = 
  let Ev (Right (rack1, bag1, gen')) = fillRack [] newBag theGen
      p1 = Player { _name  = p1Name
                  , _rack  = rack1
                  , _score = 0
                  , _isAI = False }
      Ev (Right (rack2, bag2, gen'')) = fillRack [] bag1 gen'
      p2 = Player { _name  = p2Name
                  , _rack  = rack2
                  , _score = 0
                  , _isAI  = False }
      g  = Game { _board     = newBoard
                , _bag       = bag2
                , _player1   = p1
                , _player2   = p2
                , _turn      = P1
                , _gen       = gen''
                , _firstMove = True
                , _dict      = d
                , _gameOver  = False
                , _playable  = Map.empty
                , _lastMovePass = False } in
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
     -> Evaluator (Game, MoveResult)
move validate g w is = additionalWords g w >>= \aw -> setBlanks w is g
  >>= \g' -> validate (w:aw) g' >> scoreWords g w aw
  >>= \sc -> setScore g' sc 
  >>= updatePlayer w >>= updatePlayables w >>= updateBoard w
  >>= endNonPassMove >>= checkEndOfGame <&> (,MoveResult (g ^. turn) w (map wordPutToWord aw) is sc)

-- | Take a move by swapping tiles.
swap :: Word -- ^ The tiles to swap.
     -> Game -- ^ The game.
     -> Evaluator Game
swap ls g = do
  let p      = g ^. getPlayer g
      r      = p ^. rack
      theBag = g ^. bag
      theGen = g ^. gen
  takeFromRack r ls >>= \r' -> fillRack r' theBag theGen
    >>= \(r'', theBag', theGen') -> setPlayer g (p & rack .~ r'')
    >>= endNonPassMove >>= checkEndOfGame
    >>= \g' -> pure (g' & bag .~ (ls++theBag')
                      & gen .~ theGen'
                      & lastMovePass .~ False )

-- | Take a move by passing.
pass :: Game -> Evaluator Game
pass g = if g ^. lastMovePass
         then endGame g
         else toggleTurn (g & lastMovePass .~ True ) >>= checkEndOfGame

  
