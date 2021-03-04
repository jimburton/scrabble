module Scrabble.Board.Validation
  ( validateMove
  , validateRack
  , touches
  , connects )
  where

import Data.Maybe
  ( fromJust )
import Scrabble.Types
  ( Board
  , Player(..)
  , WordPut
  , Pos
  , Rack )
import Scrabble.Evaluator
  ( Evaluator
  , evalBool )
import Scrabble.Board.Internal
  ( getSquare
  , empty
  , formatWP
  , occupiedNeighbours )

-- ================= Validation for boards ===============--

-- | Check that a move is valid: it touches at least one existing word (unless
--   it is the first move, in which case check that it touches the centre square),
--   it is in a straight and continuous line, and is made
--   up of letters that either in the rack or on the board.
validateMove :: Board    -- ^ The board
             -> Player   -- ^ The player making the move
             -> WordPut  -- ^ The word to play
             -> Bool     -- ^ Is first move
             -> Evaluator Bool
validateMove b p w fm =
  connects w b fm >>
  straight w >>
  (not fm || touches (7,7) w) `evalBool`
  "First move must touch centre square" >>
  all (\(pos,(t,_)) -> maybe (t `elem` rack p) ((==t) . fst) (getSquare b pos)) w `evalBool`
  ("Letters not in rack or not on board: " ++ formatWP w)

-- | Check that a word to be played is made of tiles that are either in the player's
--   rack or are already on the board.
validateRack :: Board
             -> Rack
             -> WordPut
             -> Evaluator Bool
validateRack b r w = someNewTiles b w >>
  all (\(pos,(t,_)) -> t `elem` r
           || (not (empty b pos) && (fst . fromJust . getSquare b) pos == t)) w
  `evalBool` ("Not all tiles in rack or on board: " ++ formatWP w)


-- | Does the word touch the pos on the board?
touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

-- | New words must touch another (apart from the first one to be played)
connects :: WordPut -- ^ The word to play
         -> Board    -- ^ The board
         -> Bool     -- ^ Is first move
         -> Evaluator Bool
connects [] _ fm     = if fm then pure True else fail "Not touching any other tile"
connects (w:ws) b fm = let (pos,_) = w in
  if (not . all null) (occupiedNeighbours b pos)
  then pure True
  else connects ws b fm

-- | Words must contain at least two tiles and must be in a straight line on the board
straight :: WordPut -> Evaluator Bool
straight (w:x:xs) = let (r,_)  = fst w
                        (r',_) = fst x
                        ps     = map fst xs
                        sel    = if r == r'-1 then fst else snd
                        f      = \(x',y') -> sel x' == sel y' - 1 in 
                      (all f . zip ps $ tail ps) `evalBool` "Not in a straight line"
straight _         = fail "Too few letters"

-- | Check that a word to be played incudes some tiles that aren't on the board.
someNewTiles :: Board -> WordPut -> Evaluator Bool
someNewTiles b w = any (empty b . fst) w `evalBool`
  ("You didn't play any new tiles: " ++ formatWP w ++ show w)
