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
  ( Turn(..)
  , newGame
  , newBoard
  , newBag
  , updateBoard
  , getPlayer
  , setPlayer )
  where

import System.Random
import Prelude hiding
  ( Word
  , words )
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Text (Text)
import Lens.Simple ((.~),(^.),(&),(%~))
import Scrabble.Types
  ( Game(..)
  , player1, player2, turn, board, score, gameOver, firstMove
  , lastMovePass
  , gen
  , bag
  , Turn(..)
  , WordPut
  , Player(..)
  , rack
  , Dict
  , Word
  , Validator )
import Scrabble.Board.Board
  ( newBoard
  , additionalWords
  , updateBoard
  , wordPutToWord
  , empty
  , scoreWords
  , rackValue )
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
                , _lastMovePass = False } in
    g

-- | Update the current player in the game. 
setPlayer :: Game -> Player -> Evaluator Game
setPlayer g p = pure $ g & getPlayer g .~ p 

-- | Get the lens for the current player in the game.
getPlayer :: Functor f => Game -> (Player -> f Player) -> Game -> f Game
getPlayer g = if g ^. turn == P1 then player1 else player2

-- | Toggle the turn in the game (between P1 and P2)
toggleTurn :: Game -- ^ The game in which to toggle the turn
           -> Evaluator Game
toggleTurn g = pure (g & turn %~ \t -> if t == P1 then P2 else P1)

-- | Checks whether this game has ended because the bag and one
--   of the racks are empty, and calls endGame if so.
checkEndOfGame :: Game -> Evaluator Game
checkEndOfGame g =
  let eog = null (g ^. bag) &&
        (null (g ^. (player1 . rack)) || null (g ^. (player2 . rack))) in
  if eog
  then endGame g
  else pure g

-- | Finishes a move that is not a pass by updating the firstMove and lastMovePass fields
--   then toggling the turn.
endNonPassMove :: Game -> Evaluator Game
endNonPassMove g = toggleTurn $ g & firstMove .~ False & lastMovePass .~ False 

-- | Ends the game and subtracts the tiles in each players rack from their score. If
--   one player has used all of their tiles the value of the opponent's tiles is added
--   to their score.
endGame :: Game -> Evaluator Game
endGame g = do
  let r1v = rackValue $ g ^. (player1 . rack)
      r2v = rackValue $ g ^. (player2 . rack)
      p1s = (g ^. (player1 . score) - r1v) + r2v
      p2s = (g ^. (player2 . score) - r2v) + r1v
  pure (g & player1 . score .~ p1s 
         & player2 . score .~ p2s 
         & gameOver .~ True )

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
move validate g w is = additionalWords g w 
  >>= \aw -> validate (w:aw) g >> scoreWords g w aw
  >>= \sc -> setScore g sc 
  >>= updatePlayer w >>= updateBoard w >>= endNonPassMove >>=
  checkEndOfGame <&> (,(map wordPutToWord (w:aw),sc))

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

-- | Update the rack of the current player and the bag.
updatePlayer :: WordPut -> Game -> Evaluator Game
updatePlayer w g = do
  let p      = g ^. getPlayer g
      r      = p ^. rack
      theBag = g ^. bag 
      theGen = g ^. gen
  takeFromRack r (map (fst . snd) (filter (\(pos,_) -> empty (g ^. board) pos) w))
    >>= \r' -> fillRack r' theBag theGen
    >>= \(r'', theBag', theGen') -> setPlayer g (p & rack .~ r'' )
    >>= \g' -> pure (g' & bag .~ theBag' & gen .~ theGen')

-- | Update the score of the current player in the game.
setScore :: Game -- ^ The game to be updated
         -> Int  -- ^ The new score of the current player
         -> Evaluator Game -- ^ The updated game.
setScore g s = pure $ g & getPlayer g . score %~ (+s)




