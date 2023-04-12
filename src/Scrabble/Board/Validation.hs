{-|
Module      : Scrabble.Board.Validation
Description : Validation of boards.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

Validation of boards.
-}
module Scrabble.Board.Validation
  ( validateMove
  , validateRack )
  where

import Control.Monad.Fail (fail)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Control.Lens ((^.))
import Scrabble.Types
  ( Board
  , Player(..)
  , rack
  , WordPut
  , Pos
  , Rack
  , Dir(..))
import Scrabble.Evaluator
  ( Evaluator
  , evalBool )
import Scrabble.Board.Internal
  ( getSquare
  , empty
  , formatWP
  , occupiedNeighbours
  , getDirection
  , onBoard ) 

-- * Validation for boards

-- | Check that a move is valid: it touches at least one existing word (unless
--   it is the first move, in which case check that it touches the centre square),
--   it is in a straight and continuous line, and is made
--   up of letters that either in the rack or on the board.
validateMove :: Board    -- ^ The board.
             -> Player   -- ^ The player making the move.
             -> WordPut  -- ^ The word to play.
             -> Bool     -- ^ Is first move?
             -> Evaluator ()
validateMove b p w fm =
  wordOnBoard w 
  >> connects w b fm
  >> straight w
  >> firstMoveTouchesCentre w fm
  >> lettersAvailable w p b

-- The letter in this move are available in the player's rack or on the board.
lettersAvailable :: WordPut -> Player -> Board -> Evaluator ()
lettersAvailable w p b = all available w
                         `evalBool` ("Letters not in rack or not on board: " <> formatWP w)
  where available (pos,(t,_)) = maybe (t `elem` (p ^. rack)) ((==t) . fst) (getSquare b pos) 
  
-- | Check that a word to be played is made of tiles that are either in the player's
--   rack or are already on the board.
validateRack :: Board     -- ^ The board.
             -> Rack      -- ^ The rack.
             -> WordPut   -- ^ The word to validate against the rack.
             -> Evaluator ()
validateRack b r w = someNewTiles b w >>
  all (\(pos,(t,_)) -> t `elem` r
           || (not (empty b pos) && (fst . fromJust . getSquare b) pos == t)) w
  `evalBool` ("Not all tiles in rack or on board: " <> formatWP w)

-- | Check that a @WordPut@ is on the board.
wordOnBoard :: WordPut -> Evaluator ()
wordOnBoard w = all (onBoard . fst) w `evalBool` "Word not on board"

-- | Does the word touch the pos on the board?
touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

-- If this is the first move, does it touch the centre square?
firstMoveTouchesCentre :: WordPut -- ^ The word.
                       -> Bool    -- ^ Is the first move.
                       -> Evaluator ()
firstMoveTouchesCentre w fm = (not fm || touches (7,7) w) `evalBool`
  "First move must touch centre square"

-- | New words must touch another (apart from the first one to be played).
connects :: WordPut -- ^ The word to play
         -> Board    -- ^ The board
         -> Bool     -- ^ Is first move
         -> Evaluator ()
connects ws b fm =
  let end = if fm then pure () else fail "Not touching any other tile" in
    foldl (\acc (pos,_) -> if (not . all null) (occupiedNeighbours b pos)
                           then pure ()
                           else acc) end ws


-- | Words must contain at least two tiles and must be in a straight line on the board.
straight :: WordPut -> Evaluator ()
straight wp | length wp > 2 =
                let ps      = map fst wp
                    (s1,s2) = if getDirection wp == VT then (fst,snd) else (snd,fst)
                    f       = \(x',y') -> s1 x' == s1 y' - 1 && s2 x' == s2 y' in 
                  (all f . zip ps $ tail ps) `evalBool` "Not in a straight line"
            | otherwise    = fail "Too few letters"

-- Check that a word to be played incudes some tiles that aren't on the board.
someNewTiles :: Board -> WordPut -> Evaluator ()
someNewTiles b w = any (empty b . fst) w `evalBool`
  ("You didn't play any new tiles: " <> formatWP w <> T.pack (show w))
