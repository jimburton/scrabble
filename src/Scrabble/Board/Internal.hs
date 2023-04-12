{-|
Module      : Scrabble.Board.Internal
Description : Functions shared by various Board-related modules.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

Functions shared by various Board-related modules.
-}
module Scrabble.Board.Internal
  ( getSquare
  , formatWP
  , occupiedNeighbours
  , wordOnRow
  , wordOnCol
  , empty
  , incRow
  , incCol
  , decRow
  , decCol
  , updateSquare
  , newTilesInMove
  , newTiles
  , freeness
  , freedomsFromWord
  , getDirection
  , adjacent
  , wordPutToWord
  , onBoard )
  where

import Prelude hiding (Word)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing, isJust)
import Data.Array ((!),(//))
import Scrabble.Types
  ( Board
  , Pos
  , Letter
  , Dir(..)
  , WordPut
  , PosTransform
  , Word
  , Tile
  , FreedomDir(..)
  , Freedom) 
import Scrabble.Lang.Word (wordToText)
  
-- * Internal for Board

-- ** Neighbours

-- Find neighbouring squares to a position on the board.
neighbours :: Pos -> [Pos]
neighbours (r,c) = filter onBoard [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

-- | Find neighbouring squares to a position on the board that are occupied.
occupiedNeighbours :: Board -- ^ The board
                   -> Pos   -- ^ The position on the board.
                   -> [Pos] -- ^ The occupied neighbours of the pos.
occupiedNeighbours b = filter (isJust . getSquare b) . neighbours 

-- | Are two positions adjacent vertically, horizontally or diagonally? 
adjacent :: Pos -> Pos -> Bool
adjacent (r1,c1) (r2,c2) = abs (r1-r2) <= 1 && abs (c1-c2) <= 1

-- Find neighbouring squares and diagonal squares to a position on the board.
--adjacents :: Pos -> [Pos]
--adjacents (r,c) = neighbours (r,c) ++
--  filter onBoard [(r-1,c-1), (r-1,c+1), (r+1,c-1), (r+1,c+1)]

-- ** Playable spaces on the board

-- | The value of a Freedom
freeness :: Freedom -> Int
freeness = snd

-- The playable spaces around an occupied position on the board.
freedom :: Dir    -- ^ The direction of the word the letter is part of.
        -> Board  -- ^ The board.
        -> Pos    -- ^ The pos.
        -> Letter -- ^ The letter on the pos.
        -> (Pos, Letter, (Freedom, Freedom))
freedom d = if d == HZ then rowFreedom else colFreedom

-- The playable space above and below this position.
rowFreedom :: Board        -- ^ The board.
           -> Pos          -- ^ The pos.
           -> Letter       -- ^ The letter on the pos.
           -> (Pos, Letter, (Freedom, Freedom))
rowFreedom b (r,c) l =
  let mins = takeWhile (\p -> canPlay b p && (fst p == 0 || canPlay b (decRow p)))
             (iterate decRow (r,c))
      minR = if null mins then r else fst (last mins)  
      maxs = takeWhile (\p -> canPlay b p && (fst p == 14 || canPlay b (incRow p)))
             (iterate incRow (r,c))
      maxR = if null maxs then r else fst (last maxs)  in
    ((r,c),l, ((UpD, r-minR), (DownD, maxR-r)))

-- The playable space to the left and right of this position.
colFreedom :: Board        -- ^ The board.
           -> Pos          -- ^ The pos.
           -> Letter       -- ^ The letter on the pos.
           -> (Pos, Letter, (Freedom, Freedom))
colFreedom b (r,c) l =
  let mins = takeWhile (\p -> canPlay b p && (fst p == 0 || canPlay b (decCol p)))
             (iterate decCol (r,c))
      minC = if null mins then c else snd (last mins)  
      maxs = takeWhile (\p -> canPlay b p && (fst p == 14 || canPlay b (incCol p)))
             (iterate incCol (r,c))
      maxC = if null maxs then c else snd (last maxs) in 
    ((r,c),l, ((LeftD, c-minC), (RightD, maxC-c)))

-- | All of the playable spaces around a word on the board.
freedomsFromWord :: WordPut -> Board -> [(Pos, Letter, (Freedom, Freedom))]
freedomsFromWord w b =
  let d  = getDirection w in 
    filter (\(_,_,(n,s)) -> freeness n > 0 || freeness s > 0)
    $ map (\(p,(l,_)) -> freedom d b p l) w

-- Is this position on the board and unoccupied?
canPlay :: Board -> Pos -> Bool
canPlay b p = onBoard p && isNothing (getSquare b p)

-- Is this position on the board, unoccupied and are all of its adjacent positions free?
--isFree :: Board -> Pos -> Bool
--isFree b p = canPlay b p && not (any (isJust . getSquare b) (adjacents p))

-- | Format a WordPut as Text.
formatWP :: WordPut -> Text
formatWP w = wordToText (map (fst . snd) w) <> ": " <> T.pack (show (fst (head w)))
             <> " " <> T.pack (show (getDirection w))

-- | Convert a WordPut to Word.
wordPutToWord :: WordPut -> Word
wordPutToWord = map (fst . snd)

-- ** Manipulating and querying the board

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
          -> WordPut
wordOnRow b pos = wordFromSquare b incCol (startOfWord b decCol pos)

-- | Retrieve the word that crosses a position on the board vertically.
wordOnCol :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> WordPut
wordOnCol b pos = wordFromSquare b incRow (startOfWord b decRow pos)

-- | How many new tiles are being played in a move?
newTilesInMove :: Board -> WordPut -> Int
newTilesInMove b = length . newTiles b 

-- | The new tiles that are being played in a move.
newTiles :: Board -> WordPut -> [(Pos, Tile)]
newTiles b = filter (isNothing. getSquare b . fst)

-- Retrieve the word that crosses this pos on the board
wordFromSquare :: Board        -- ^ The board.
               -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
               -> Pos          -- ^ The pos.
               -> WordPut
wordFromSquare b f pos = reverse $ wordFromSquare' pos []
  where wordFromSquare' p wp = case getSquare b p of
          Nothing -> wp
          Just t  -> wordFromSquare' (f p) ((p,t):wp)

-- Find the starting position of a word that crosses a position on the board.
startOfWord :: Board        -- ^ The board.
            -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
            -> Pos          -- ^ The position
            -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'
