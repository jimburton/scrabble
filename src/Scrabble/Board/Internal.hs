module Scrabble.Board.Internal
  ( getSquare
  , formatWP
  , occupiedNeighbours
  , vNeighbours
  , hNeighbours
  , wordOnRow
  , wordOnCol
  , empty
  , incRow
  , incCol
  , decRow
  , decCol
  , updateSquare
  , newTilesInMove
  , freedomsFromWord
  , getDirection
  , newTiles
  , adjacent
  , wordPutToWord )
  where

import Debug.Trace
import Prelude hiding (Word)
import Data.Maybe
  ( isNothing
  , isJust )
import Data.Array
  ( (!)
  , (//) )
import Scrabble.Types
  ( Board
  , Pos
  , Letter
  , Dir(..)
  , WordPut
  , PosTransform
  , Word
  , Tile ) 
import Scrabble.Lang.Word
  ( wordToString )
  
-- ======== Internal for Board ========== --

-- ======== Neighbours ================== --

-- | Find neighbouring squares to a position on the board.
neighbours :: Pos -> [Pos]
neighbours (r,c) = filter (\(x,y) -> x >= 0 && x < 15 && y >= 0 && y < 15)
  [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

-- | Find neighbouring squares to a position on the board that are occupied.
occupiedNeighbours :: Board -- ^ The board
                   -> Pos   -- ^ The position on the board.
                   -> [Pos]
occupiedNeighbours b pos = filter (isJust . getSquare b) $ neighbours pos

-- | The occupied horizonal neighbours of a position on the board.
hNeighbours :: Board -> Pos -> [Pos]
hNeighbours = gridNeighbours decRow incRow

-- | The occupied vertical neighbours of a position on the board.
vNeighbours :: Board -> Pos -> [Pos]
vNeighbours = gridNeighbours decCol incCol

-- | Get the vertical or horizontal neighbours of a pos
gridNeighbours :: PosTransform -- ^ Seek in first direction (left or up)
               -> PosTransform -- ^ Seek in second direction (right or down)
               -> Board        -- ^ The board
               -> Pos          -- ^ The pos
               -> [Pos]
gridNeighbours f g b pos = let l = [f pos | isJust (getSquare b (f pos))]
                               m = [g pos | isJust (getSquare b (g pos))] in
                        l ++ m

-- | Are two positions adjacent vertically, horizontally or diagonally? 
adjacent :: Pos -> Pos -> Bool
adjacent (r1,c1) (r2,c2) = abs (r1-r2) <= 1 && abs (c1-c2) <= 1

-- ========== Playable spaces on the board ================== --

-- ^ The playable spaces around an occupied position on the board.
freedom :: Board  -- ^ The board.
        -> Pos    -- ^ The pos.
        -> Letter -- ^ The letter on the pos.
        -> Dir    -- ^ The direction of the word the letter is part of.
        -> (Pos, Letter, (Int, Int))
freedom b p l d =
  if d == HZ
  then freedomFrom incRow decRow (\(r,_) (n,s) -> (r-n,s-r)) b p l 
  else freedomFrom incCol decCol (\(_,c) (n,s) -> (c-n,s-c)) b p l 

-- | The playable space above and below or to the left and right of this position.
freedomFrom :: PosTransform -- ^ Incrementer for the pos.
            -> PosTransform -- ^ Decrementer for the pos.
            -> (Pos -> Pos -> Pos) -- ^ Transforms initial pos into max freedom.
            -> Board        -- ^ The board.
            -> Pos          -- ^ The pos.
            -> Letter       -- ^ The letter on the pos.
            -> (Pos, Letter, (Int, Int))
freedomFrom inc dec f b (r,c) l =
  let ns = takeWhile (\p -> canPlay b p && (snd p == 0 || canPlay b (dec p)))
           (iterate dec (r,c))
      n  = if null ns then 0 else snd (last ns)
      ss = takeWhile (\p -> canPlay b p && (snd p == 14 || canPlay b (inc p)))
           (iterate inc (r,c))
      s  = if null ss then 0 else snd (last ss) in
    ((r,c),l, f (r,c) (n,s))

-- | All of the playable spaces around a word on the board.
freedomsFromWord :: WordPut -> Board -> [(Pos, Letter, (Int, Int))]
freedomsFromWord w b =
  let nt = newTiles b w 
      d  = getDirection w 
      fs = filter (\(_,_,(n,s)) -> n>0 || s>0) (map (\(p,(l,_)) -> freedom b p l d) nt) in
    fs

-- | Is this position playable?
canPlay :: Board -> Pos -> Bool
canPlay b p = onBoard p && isNothing (getSquare b p)

formatWP :: WordPut -> String
formatWP w = wordToString (map (fst . snd) w) ++ ": " ++ show (fst (head w))
             ++ " " ++ show (getDirection w)

-- | Convert a WordPut to a Word.
wordPutToWord :: WordPut -> Word
wordPutToWord = map (fst . snd)

-- ==================== Manipulating and querying the board =================--

-- | Retrieve a position on the board.
getSquare :: Board -> Pos -> Maybe Tile
getSquare b pos = if onBoard pos
                    then b ! pos
                    else Nothing

-- | Place a tile onto the board.
updateSquare :: Board -> (Pos, Tile) -> Board
updateSquare b (pos,t) = b // [(pos, Just t)]

-- | Get direction of a word on the board. WordPuts must be at least two tiles
--   long.
getDirection :: WordPut -> Dir
getDirection w = let r1 = fst $ fst $ head w
                     r2 = fst $ fst $ head (tail w) in
                   if  r1<r2 then VT else HZ
  
-- | Is a square empty?
empty :: Board -> Pos -> Bool
empty b pos = isNothing (getSquare b pos)

-- | Is a position on the board?
onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r < 15 && c >= 0 && c < 15

-- | Increment the row within a position.
incRow :: PosTransform
incRow (r,c) = (r+1,c)

-- | Increment the column within a position.
incCol :: PosTransform
incCol (r,c) = (r,c+1)

-- | Decrement the row within a position.
decRow :: PosTransform
decRow (r,c) = (r-1,c)

-- | Decrement the column within a position.
decCol :: PosTransform
decCol (r,c) = (r,c-1)

-- | Retrieve the word that crosses a position on the board horizontally.
wordOnRow :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> Maybe WordPut
wordOnRow b pos = wordFromSquare b incCol (startOfWord b decCol pos)

-- | Retrieve the word that crosses a position on the board vertically.
wordOnCol :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> Maybe WordPut
wordOnCol b pos = wordFromSquare b incRow (startOfWord b decRow pos)

-- | How many new tiles are being played in a move?
newTilesInMove :: Board -> WordPut -> Int
newTilesInMove b w = length $ newTiles b w

-- | The new tiles that are being played in a move.
newTiles :: Board -> WordPut -> [(Pos, (Letter,Int))]
newTiles b = filter (\(p,_) -> isNothing (getSquare b p))  

-- | Retrieve the word that crosses this pos on the board
wordFromSquare :: Board        -- ^ The board.
               -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
               -> Pos          -- ^ The pos.
               -> Maybe WordPut
wordFromSquare b f pos = getSquare b pos >>= \t -> ((pos,t) :) <$> wordFromSquare b f (f pos)

-- | Find the starting position of a word that crosses a position on the board.
startOfWord :: Board        -- ^ The board.
            -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
            -> Pos          -- ^ The position
            -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'
