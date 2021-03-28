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
  , newBoard
  , bonusMap
  , showBoard
  , getDirection
  , wordOnRow
  , wordOnCol
  , incRow
  , incCol
  , getSquare )
  where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.Split (chunksOf)
import Data.Maybe (isNothing)
import Data.Array
import Scrabble.Dict (letterToChar)
import Scrabble.Types
  ( Board
  , Pos
  , WordPut
  , Bonus(..)
  , Tile
  , Letter
  , Dir(..)
  , PosTransform ) 

-- =============== Boards ================== --

-- | A new, empty board.
newBoard :: Board
newBoard = array ((0,0),(14,14)) [((i,j), Nothing) | i <- [0..14], j <- [0..14]]

-- | Place a word onto the board.
updateBoard :: WordPut -> Board -> Board
updateBoard w b = foldl updateSquare b w

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

-- | Get direction of a word on the board. WordPuts must be at least two tiles
--   long.
getDirection :: WordPut -> Dir
getDirection w = let r1 = fst $ fst $ head w
                     r2 = fst $ fst $ head (tail w) in
                   if  r1<r2 then VT else HZ

-- * Bonuses

-- Data for the bonus map.
bonusSquaresList :: [(Pos, Bonus)] -- ((Row, Column), Bonus)
bonusSquaresList =
  [((0, 0),    W3), ((0, 3),   L2)
  , ((0, 7),   W3), ((0, 11),  L2)
  , ((0, 14),  W3), ((1, 1),   W2)
  , ((1, 5),   L3), ((1, 9),   L3)
  , ((1, 13),  W2), ((2, 2),   W2)
  , ((2, 6),   L2), ((2, 8),   L2)
  , ((2, 12),  W2), ((3, 0),   L2)
  , ((3, 3),   W2), ((3, 7),   L2)
  , ((3, 11),  W2), ((3, 14),  L2)
  , ((4, 4),   W2), ((4, 10),  W2)
  , ((5, 1),   L3), ((5, 5),   L3)
  , ((5, 9),   L3), ((5, 13),  L3)
  , ((6, 2),   L2), ((6, 6),   L2)
  , ((6, 8),   L2), ((6, 12),  L2)
  , ((7, 0),   W3), ((7, 3),   L2)
  , ((7, 7),   W2), ((7, 11),  L2)
  , ((7, 14),  W3), ((8, 2),   L2)
  , ((8, 6),   L2), ((8, 8),   L2)
  , ((8, 12),  L2), ((9, 1),   L3)
  , ((9, 5),   L3), ((9, 9),   L3)
  , ((9, 13),  L3), ((10, 4),  W2)
  , ((10, 10), W2), ((11, 0),  L2)
  , ((11, 3),  W2), ((11, 7),  L2)
  , ((11, 11), W2), ((11, 14), L2)
  , ((12, 2),  W2), ((12, 6),  L2)
  , ((12, 8),  L2), ((12, 12), W2)
  , ((13, 1),  W2), ((13, 5),  L3)
  , ((13, 9),  L3), ((13, 13), W2)
  , ((14, 0),  W3), ((14, 3),  L2)
  , ((14, 7),  W3), ((14, 11), L2)
  , ((14, 14), W3)]

-- | Map containing the bonus squares on the board.
bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList

-- * Printing the board

-- Get all rows from the board
rows :: Board -> [[Maybe Tile]]
rows b = chunksOf 15 (elems b) 

-- | Textify a board.
showBoard :: Bool  -- ^ Whether to show bonus squares.
          -> Board -- ^ The board.
          -> Text  -- ^ The text representation of the board.
showBoard printBonuses b = topNumbers <> top <> showRows <> bottom where
  showRows      = T.intercalate "\n" (zipWith showRow [0..14] (rows b)) <> "\n"
  showRow     i r = showI i <> "|" <>
                    T.concat (zipWith (showSquare i) [0..14] r)
  showSquare :: Int -> Int -> Maybe (Letter,Int) -> Text
  showSquare i c s = case s of
                       Nothing    ->
                         if printBonuses
                         then case Map.lookup (i,c) bonusMap of
                                Nothing -> "  |"
                                Just b' -> T.pack (show b') <> "|"
                         else "  |"
                       Just (t,_) -> T.pack [' ', letterToChar t, '|']
  topNumbers    = "  |" <> T.concat (map (\i -> showI i <> "|") [0..14]) <> "\n"
  showI         :: Int -> Text
  showI i       = if i < 10 then " " <> T.pack (show i) else T.pack (show i)
  top           = line '-'
  bottom        = line '-'
  line        c = T.pack (replicate 48 c) <> "\n"
