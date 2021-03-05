module Scrabble.Game.Internal
  ( setBlanks
  , setScore
  , updatePlayables
  , getPlayer
  , setPlayer 
  , toggleTurn
  , updatePlayer )
  where

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
  , Dir(..) )
import Scrabble.Board.Board
  ( empty
  , freedomsFromWord
  , getDirection
  , newTiles
  , replace )
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

-- | Update the list of playable positions on the board based on the placement
--   of this word like so:
--   + a horizontal word may add some playable positions to the board in the N and S directions,
--   + a vertical word may add some playable positions to the board in the E and W directions,
--   + a horizontal word may reduce the playability of positions in the same columns,
--   + a vertical word may reduce the playability of positions in the same rows.
updatePlayables :: WordPut -> Game -> Evaluator Game
updatePlayables w g = do
  let ps  = playable g
      b   = board g
      nt  = newTiles b w
      ot  = map fst (w \\ nt )
      ps' = Map.filterWithKey (\k _ -> k `notElem` ot) ps 
      d   = getDirection w
      fs  = freedomsFromWord nt b
      f (u,p) = if d == HZ
                then [(UpD,u), (DownD, p)]
                else [(RightD, p), (LeftD, u)]
      nps = foldl (\acc (p,l,(n,s)) -> Map.insert p (l,f (n,s)) acc) ps' fs
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
toggleTurn g = let eog = null (bag g) 
                     && null (rack (player1 g)) 
                     && null (rack (player2 g)) in
  pure g { turn = if turn g == P1 then P2 else P1 , gameOver = eog}

  

