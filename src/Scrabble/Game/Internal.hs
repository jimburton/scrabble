{-|
Module      : Scrabble.Game.Internal
Description : Internal functions shared by several game-related modules.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Internal functions shared by several game-related modules.
-}
module Scrabble.Game.Internal
  ( setBlanks
  , setScore
  , updatePlayables
  , getPlayer
  , setPlayer 
  , toggleTurn
  , updatePlayer
  , checkEndOfGame
  , endNonPassMove
  , endGame )
  where

import qualified Data.Map as Map
import Control.Lens ((&),(^.),(.~),(%~))
import Scrabble.Types
  ( WordPut
  , Game
  , player1
  , player2
  , gameOver
  , firstMove
  , lastMovePass
  , bag
  , playable
  , board
  , turn
  , gen
  , Evaluator
  , Rack
  , Player
  , score
  , rack
  , Turn(..)
  , Letter(Blank))
import Scrabble.Board.Board
  ( rackValue
  , adjacent
  , empty
  , freedomsFromWord
  , freeness 
  , newTiles )
import Scrabble.Board.Bag 
  ( fillRack
  , takeFromRack )
import Scrabble.Evaluator ()

-- ======== Internal for games ========= --

-- | Update the current player's rack so that any blanks which have been played
--   are exchanged with the letters the player wants them to stand for.
setBlanks :: WordPut -- ^ Word that had blanks in it.
          -> [Int]   -- ^ Positions of the letters in the word which were blanks.
          -> Game    -- ^ The game.
          -> Evaluator Game -- ^ The updated game.
setBlanks w bs g = pure (getPlayer g)
                   >>= \p -> pure (g ^. p & rack %~ setBlanks' bs)
                   >>= setPlayer g 
  where setBlanks' :: [Int] -> Rack -> Rack
        setBlanks' bs' r = foldl (\acc i -> replaceBlank acc (fst (snd (w !! i)))) r bs'
        replaceBlank :: Rack -> Letter -> Rack
        replaceBlank [] _         = []
        replaceBlank (Blank:rs) l = l : rs
        replaceBlank (r:rs) l     = r : replaceBlank rs l

-- | Update the score of the current player in the game.
setScore :: Game -- ^ The game to be updated
         -> Int  -- ^ The new score of the current player
         -> Evaluator Game -- ^ The updated game.
setScore g s = pure $ g & getPlayer g . score %~ (+s)

-- | Update the list of playable positions on the board based on the placement
--   of this word like so:
--   + a horizontal word may add some playable positions to the board in the UpD and DownD directions,
--   + a vertical word may add some playable positions to the board in the LeftD and RightD- directions,
--   + a horizontal word may reduce the playability of positions in the same columns,
--   + a vertical word may reduce the playability of positions in the same rows.
updatePlayables :: WordPut -> Game -> Evaluator Game
updatePlayables w g = do
  let ps  = g ^. playable
      b   = g ^. board 
      nt  = newTiles b w
      ntp = map fst nt
      -- assuming any existing playable that is adjacent to the new word
      -- will no longer be playable.
      ps' = Map.filterWithKey (\k _ -> not (any (adjacent k) ntp))  ps
      fs  = freedomsFromWord nt b
      f (u,p) = filter ((>0) . freeness) [u, p]
      nps = foldl (\acc (p,l,(f1,f2)) -> Map.insert p (l,f (f1,f2)) acc) ps' fs
  pure (g & playable .~ nps)

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

  

