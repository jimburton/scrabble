{-|
Module      : Scrabble.Board.Board
Description : Functions for the Scrabble board.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for the Scrabble board.
-}
module Scrabble.Board
  ( updateBoard
  , updateSquare
  , getSquare
  , newBoard
  , getDirection
  , wordOnRow
  , wordOnCol
  , incRow
  , incCol )
  where

import Data.Maybe (isNothing)
import Data.Array
import Lens.Simple ((^.),(.~),(&))
import Scrabble.Types
  ( Board
  , Pos
  , WordPut
  , Tile
  , Dir(..)
  , PosTransform
  , Game
  , board) 

-- * Creating and querying boards

-- | A new, empty board.
newBoard :: Board
newBoard = array ((0,0),(14,14)) [((i,j), Nothing) | i <- [0..14], j <- [0..14]]

-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Game
updateBoard w g = g & board .~ foldl updateSquare (g ^. board) w

-- | Place a tile onto the board.
updateSquare :: Board -> (Pos, Tile) -> Board
updateSquare b (pos,t) = b // [(pos, Just t)]

-- | Is a position on the board?
onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r < 15 && c >= 0 && c < 15

-- | Retrieve a position on the board.
getSquare :: Board -> Pos -> Maybe Tile
getSquare b pos = if onBoard pos
                    then b ! pos
                    else Nothing

-- | Get direction of a word on the board. WordPuts must be at least two tiles
--   long.
getDirection :: WordPut -> Dir
getDirection w = let r1 = fst $ fst $ head w
                     r2 = fst $ fst $ head (tail w) in
                   if  r1<r2 then VT else HZ

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

-- Find the starting position of a word that crosses a position on the board.
startOfWord :: Board        -- ^ The board.
            -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
            -> Pos          -- ^ The position
            -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'


-- Retrieve the word that crosses this pos on the board
wordFromSquare :: Board        -- ^ The board.
               -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
               -> Pos          -- ^ The pos.
               -> WordPut
wordFromSquare b f pos = reverse $ wordFromSquare' pos []
  where wordFromSquare' p wp = case getSquare b p of
          Nothing -> wp
          Just t  -> wordFromSquare' (f p) ((p,t):wp)

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

