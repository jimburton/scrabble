{-|
Module      : Scrabble.Board.Board
Description : Functions for the Scrabble board.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for the Scrabble board.
-}
module Scrabble.Board.Board
  ( updateBoard
  , updateSquare
  , getSquare
  , empty
  , onBoard
  , newBoard
  , getDirection
  , wordOnRow
  , wordOnCol
  , incRow
  , incCol
  , occupiedNeighbours
  , formatWP
  , additionalWords
  , wordPutToWord )
  where

import Prelude hiding (Word)
import Data.Maybe
  ( isNothing
  , isJust )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Array
import Lens.Simple ((^.),(.~),(&))
import Scrabble.Types
  ( Board
  , Pos
  , Word
  , WordPut
  , Tile
  , Dir(..)
  , PosTransform
  , Game
  , board
  , Evaluator )
import Scrabble.Lang.Word (wordToText)
import Scrabble.Evaluator() -- for the instances.

-- * Creating and querying boards

-- | A new, empty board.
newBoard :: Board
newBoard = array ((0,0),(14,14)) [((i,j), Nothing) | i <- [0..14], j <- [0..14]]

-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Evaluator Game
updateBoard w g = pure (g & board .~ foldl updateSquare (g ^. board) w) 

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
-- | Is a square empty?
empty :: Board -> Pos -> Bool
empty b pos = isNothing (getSquare b pos)

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

-- Find neighbouring squares to a position on the board.
neighbours :: Pos -> [Pos]
neighbours (r,c) = filter onBoard [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

-- | Find neighbouring squares to a position on the board that are occupied.
occupiedNeighbours :: Board -- ^ The board
                   -> Pos   -- ^ The position on the board.
                   -> [Pos] -- ^ The occupied neighbours of the pos.
occupiedNeighbours b = filter (isJust . getSquare b) . neighbours

-- | Format a WordPut as Text.
formatWP :: WordPut -> Text
formatWP w = wordToText (map (fst . snd) w) <> ": " <> T.pack (show (fst (head w)))
             <> " " <> T.pack (show (getDirection w))

-- | Find the additional words that are created by placing a word on the board.
additionalWords :: Game    -- ^ The game.
                -> WordPut -- ^ The new word.
                -> Evaluator [WordPut] -- ^ The list of additional words.
additionalWords g w = updateBoard w g >>= \g' -> do
  let oldb   = g  ^. board
      newb   = g' ^. board
      oppDir = if getDirection w == HZ then VT else HZ
      nt     = map fst (newTiles oldb w) 
      mWds   = if oppDir == HZ then map (wordOnRow newb) nt else  map (wordOnCol newb) nt
  pure $ filter ((>1) . length) mWds
             
-- | The new tiles that are being played in a move.
newTiles :: Board -> WordPut -> [(Pos, Tile)]
newTiles b = filter (\(p,_) -> isNothing (getSquare b p))

-- | Convert a WordPut to Word.
wordPutToWord :: WordPut -> Word
wordPutToWord = map (fst . snd)


