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
  , newTiles )
  where

import Debug.Trace
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
  , PosTransform )
import Scrabble.Lang.Word
  ( wordToString )
-- ======== Internal for Board ========== --

-- | The playable space above and below or to the left and right of this position.
freedomFromRow :: Board -> Pos -> Letter -> (Pos, Letter, (Int, Int))
freedomFromRow b (r,c) l =
  let ns = takeWhile (\p -> canPlay b p && (fst p == 0 || canPlay b (decRow p)))
           (iterate decRow (r,c))
      n  = if null ns then 0 else fst (last ns)
      ss = takeWhile (\p -> canPlay b p && (fst p == 14 || canPlay b (incRow p)))
           (iterate incRow (r,c))
      s  = if null ss then 0 else fst (last ss) in
    ((r,c),l,(r-n,s-r))

-- | The playable space above and below or to the left and right of this position.
freedomFromCol :: Board -> Pos -> Letter -> (Pos, Letter, (Int, Int))
freedomFromCol b (r,c) l =
  let ns = takeWhile (\p -> canPlay b p && (snd p == 0 || canPlay b (decCol p)))
           (iterate decCol (r,c))
      n  = if null ns then 0 else snd (last ns)
      ss = takeWhile (\p -> canPlay b p && (snd p == 14 || canPlay b (decCol p)))
           (iterate incCol (r,c))
      s  = if null ss then 0 else snd (last ss) in
    ((r,c),l,(c-n,s-c))

freedom :: Board -> Pos -> Letter -> Dir -> (Pos, Letter, (Int, Int))
freedom b p l d = if d == HZ
                  then freedomFromRow b p l
                  else freedomFromCol b p l

-- | Is this position playable?
canPlay :: Board -> Pos -> Bool
canPlay b p = onBoard p && isNothing (getSquare b p)

freedomsFromWord :: WordPut -> Board -> [(Pos, Letter, (Int, Int))]
freedomsFromWord w b =
  let nt = newTiles b w 
      d  = getDirection w 
      fs = filter (\(_,_,(n,s)) -> n>0 || s>0) (map (\(p,(l,_)) -> freedom b p l d) nt) in
    (trace ("freedoms: "++(show fs))) $ fs

-- ==================== Manipulating and querying the board =================--

-- | Retrieve a position on the board.
getSquare :: Board -> Pos -> Maybe (Letter,Int)
getSquare b (r,c) = if onBoard (r,c)
                    then (b ! r) ! c
                    else Nothing

-- | Place a tile onto the board.
updateSquare :: Board -> (Pos, (Letter,Int)) -> Board
updateSquare b ((r,c),l) = let row = (b ! r) // [(c, Just l)] in
                             b // [(r, row)]

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

-- | Find neighbouring squares to a position on the board.
neighbours :: Pos -> [Pos]
neighbours (r,c) = filter (\(x,y) -> x >= 0 && x < 15 && y >= 0 && y < 15)
  [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

-- | Find neighbouring squares to a position on the board that are occupied.
occupiedNeighbours :: Board -- ^ The board
                   -> Pos   -- ^ The position on the board.
                   -> [Pos]
occupiedNeighbours b pos = filter (isJust . getSquare b) $ neighbours pos

-- | Retrieve the word that crosses a position on the board horizontally.
wordOnRow :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> WordPut
wordOnRow b (r,c) = wordFromSquare b incCol (startOfWord b decCol (r,c))

-- | Retrieve the word that crosses a position on the board vertically.
wordOnCol :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> WordPut
wordOnCol b (r,c) = wordFromSquare b incRow (startOfWord b decRow (r,c))

-- | How many new tiles are being played in a move?
newTilesInMove :: Board -> WordPut -> Int
newTilesInMove b w = length $ newTiles b w

-- | The new tiles that are being played in a move.
newTiles :: Board -> WordPut -> [(Pos, (Letter,Int))]
newTiles b = filter (\(p,_) -> isNothing (getSquare b p))  


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
-- | Retrieve the word that crosses this pos on the board
wordFromSquare :: Board
               -> PosTransform -- ^ The function that moves to the start of the word (up rows or left along columns)
               -> Pos
               -> WordPut
wordFromSquare b f pos =  maybe [] (\t -> (pos, t) : wordFromSquare b f (f pos)) (getSquare b pos)

-- | Find the starting position of a word that crosses a position on the board.
startOfWord :: Board        -- ^ The board.
            -> PosTransform -- ^ The function that moves to the start of the word (up rows or left along columns)
            -> Pos          -- ^ The position
            -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'

formatWP :: WordPut -> String
formatWP w = wordToString (map (fst . snd) w) ++ ": " ++ show (fst (head w))
             ++ " " ++ show (getDirection w)
