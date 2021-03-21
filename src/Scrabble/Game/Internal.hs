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

import Debug.Trace
import qualified Data.Map as Map
import Data.List ( (\\) )
import Scrabble.Types
  ( WordPut
  , Game(..)
  , Evaluator
  , Rack
  , Player(..)
  , Turn(..)
  , FreedomDir(..)
  , Dir(..)
  , Letter(Blank))
import Scrabble.Board.Board
  ( empty
  , freedomsFromWord
  , getDirection
  , newTiles
  , rackValue )
import Scrabble.Board.Internal 
  ( adjacent )
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
          -> Evaluator Game
setBlanks w bs g = pure (getPlayer g)
                   >>= \p -> pure (p { rack = setBlanks' (rack p) bs})
                   >>= setPlayer g 
  where setBlanks' :: Rack -> [Int] -> Rack
        setBlanks' = foldl (\acc i -> replaceBlank acc (fst (snd (w !! i))))
        replaceBlank :: Rack -> Letter -> Rack
        replaceBlank [] _         = []
        replaceBlank (Blank:rs) l = l : rs
        replaceBlank (r:rs) l     = r : replaceBlank rs l

-- | Update the score of the current player in the game.
setScore :: Game -- ^ The game to be updated
         -> Int  -- ^ The new score of the current player
         -> Evaluator Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                      pure g { player1 = (player1 g) {score = s' + s} }
               else let s' = score (player2 g) in
                      pure g { player2 = (player2 g) {score = s' + s} }

-- | Update the list of playable positions on the board based on the placement
--   of this word like so:
--   + a horizontal word may add some playable positions to the board in the UpD and DownD directions,
--   + a vertical word may add some playable positions to the board in the LeftD and RightD- directions,
--   + a horizontal word may reduce the playability of positions in the same columns,
--   + a vertical word may reduce the playability of positions in the same rows.
updatePlayables :: WordPut -> Game -> Evaluator Game
updatePlayables w g = do
  let ps  = playable g
      b   = board g
      nt  = newTiles b w
      ntp = map fst nt
      -- assuming any existing playable that is adjacent to the new word
      -- will no longer be playable.
      ps' = Map.filterWithKey (\k _ -> not (any (adjacent k) ntp))  ps
      d   = getDirection w
      fs  = freedomsFromWord nt b
      f (u,p) = if d == HZ
                then filter ((>0) . snd) [(UpD,u), (DownD, p)]
                else filter ((>0) . snd) [(LeftD, u), (RightD, p)]
      nps = foldl (\acc (p,l,(n,s)) -> Map.insert p (l,f (n,s)) acc) ps' fs
  (trace $ "Playables: \n" ++ show nps) pure (g { playable = nps })

-- | Update the rack of the current player and the bag.
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

-- | Checks whether this game has ended because the bag and one
--   of the racks are empty, and calls endGame if so.
checkEndOfGame :: Game -> Evaluator Game
checkEndOfGame g =
  let eog = null (bag g) &&
        (null (rack (player1 g)) || null (rack (player2 g))) in
  if eog
  then endGame g
  else pure g

-- | Finishes a move that is not a pass by updating the firstMove and lastMovePass fields
--   then toggling the turn.
endNonPassMove :: Game -> Evaluator Game
endNonPassMove g = toggleTurn $ g { firstMove = False, lastMovePass = False }

-- | Ends the game and subtracts the tiles in each players rack from their score. If
--   One player has used all of their tiles the value of the opponent's tiles is added
--   to their score.
endGame :: Game -> Evaluator Game
endGame g = do
  let r1v = rackValue (rack (player1 g))
      r2v = rackValue (rack (player1 g))
      p1s = (score (player1 g) - r1v) + r2v
      p2s = (score (player2 g) - r2v) + r1v
  pure g { player1 = (player1 g) { score = p1s }
         , player2 = (player2 g) { score = p2s }
         , gameOver = True }

  

