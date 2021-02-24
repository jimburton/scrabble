module Scrabble.Board.Board ( Board
                            , WordPut
                            , Dir(..)
                            , Player(..)
                            , Rack
                            , Pos
                            , Bonus(..)
                            , validateMove
                            , touches
                            , connects
                            , straight
                            , getSquare
                            , updateSquare
                            , updateBoard
                            , newBoard
                            , scoreLetter
                            , bonusMap
                            , scoreWord
                            , validateRack
                            , numTilesList
                            , incCol
                            , incRow
                            , empty
                            , additionalWords
                            , mkWP )
  where

import qualified Data.Map as Map
import Data.Maybe                ( fromJust
                                 , isNothing
                                 , catMaybes
                                 , isJust )
import Data.Array
import Data.Map                  ( Map )
import Scrabble.Letter           ( Letter(..)
                                 , toChar
                                 , charToLetterMap )
import Scrabble.Rack             ( Rack )

type Board = Array Int (Array Int (Maybe Letter))

data Player = Player { name :: String
                     , rack :: Rack
                     , score :: Int
                     } deriving (Show, Eq)

newBoard :: Board
newBoard = listArray (0,14) $ replicate 15 (listArray (0,14) $ replicate 15 Nothing)

-- private value.
letterToScoreList :: [(Letter,Int)]
letterToScoreList = [
  (A, 1), (B, 3), (C, 3), (D, 2), (E, 1), (F, 4), (G, 2),
  (H, 4), (I, 1), (J, 8), (K, 5), (L, 1), (M, 3), (N, 1),
  (O, 1), (P, 3), (Q, 10), (R, 1), (S, 1), (T, 1), (U, 1),
  (V, 4), (W, 4), (X, 8), (Y, 4), (Z, 10), (Blank, 0) ]

-- private value.
letterToScoreMap :: Map Letter Int
letterToScoreMap = Map.fromList letterToScoreList

scoreLetter :: Letter -> Int
scoreLetter = fromJust . flip Map.lookup letterToScoreMap

showTile :: Letter -> String
showTile l = [toChar l] ++  " (" ++ show (scoreLetter l) ++ ")"

numTilesList :: [(Letter,Int)]
numTilesList = [
  (A, 9), (B, 2), (C, 2), (D, 4), (E, 12), (F, 2), (G, 3),
  (H, 2), (I, 9), (J, 1), (K, 1), (L, 4), (M, 2), (N, 6),
  (O, 8), (P, 2), (Q, 1), (R, 6), (S, 4), (T, 6), (U, 4),
  (V, 2), (W, 2), (X, 1), (Y, 2), (Z, 1), (Blank, 2) ]

type Pos = (Int, Int)

type WordPut = [(Pos, Letter)]

data Dir = HZ | VT deriving (Show, Read, Eq)

-- | The Bool argument is whether any bonus should be applied to this
--   Pos and Tile, i.e. whether this is the first time it has been
--   counted.
scoreWord :: [(Pos, Letter, Bool)] -> Int
scoreWord = scoreWord' 0 1 where
  scoreWord' s b [] = s * b
  scoreWord' s b ((pos,t,p):ws) =
    if not p then scoreWord' (scoreLetter t + s) b ws
    else case Map.lookup pos bonusMap of
      Nothing -> scoreWord' (scoreLetter t + s) b ws
      Just b'  -> case b' of
                   (Word i)   -> scoreWord' (scoreLetter t + s) (i*b) ws
                   (Letter i) -> scoreWord' ((scoreLetter t * i) + s) b ws

-- ================= Validation ===============--

validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
validateMove b p w fm = case connects w b fm of
  Right _ -> case straight w of
               Right _ -> if all (\(pos,t) -> case getSquare b pos of
                                     Just l  -> l == t
                                     Nothing -> t `elem` rack p) w
                          then if fmGood
                               then Right True
                               else Left "First move must touch centre square"
                          else Left "Letters not in rack or not on board"
               Left e -> Left e
  Left e -> Left e
  where fmGood = not fm || touches (7,7) w 


touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

connects :: WordPut -- ^ The word to play
         -> Board   -- ^ The board
         -> Bool    -- ^ Is first move
         -> Either String Bool
connects [] _ fm     = if fm then Right True else Left "Not touching any other tile"
connects (w:ws) b fm = let (pos,_) = w in
  if (not . all null) (occupiedNeighbours b pos)
  then Right True
  else connects ws b fm

-- | WordPuts must have at least two elements 
straight :: WordPut -> Either String Bool
straight (w:x:xs) = let (r,_)  = fst w
                        (r',_) = fst x
                        ps     = map fst xs in 
                    if r == r' - 1
                    then if all (\(x',y') -> fst x' == fst y' - 1) . zip ps $ tail ps
                         then Right True
                         else Left "Not in a straight line"
                    else if all (\(x',y') -> snd x' == snd y' - 1) . zip ps $ tail ps
                         then Right True
                         else Left "Not in a straight line"
straight _         = Left "Too few letters"

validateRack :: Board
             -> Rack
             -> WordPut
             -> Either String Bool
validateRack b r w = case someNewTiles b w of
  Right _ -> if all (\(pos,t) -> t `elem` r
                      || (not (empty b pos) && (fromJust . getSquare b) pos == t)) w
             then Right True
             else Left "Not all tiles in rack or on board"
  Left e -> Left e

someNewTiles :: Board
             -> WordPut
             -> Either String Bool
someNewTiles b w = if any (empty b . fst) w
                   then Right True
                   else Left "You didn't play any new tiles"

-- ==================== Manipulating and querying the board =================--

getSquare :: Board -> Pos -> Maybe Letter
getSquare b (r,c) = if onBoard (r,c)
                    then (b ! r) ! c
                    else Nothing

updateSquare :: Board -> (Pos, Letter) -> Board
updateSquare b ((r,c),l) = let row = (b ! r) // [(c, Just l)] in
                             b // [(r, row)]

updateBoard :: WordPut -> Board -> Board
updateBoard ws b = foldl updateSquare b ws

wordString :: WordPut -> String
wordString = map (toChar . snd)

additionalWords :: Board -> WordPut -> [WordPut]
additionalWords _ [] = []
additionalWords b (wp:wps) =
  let (r,c) = fst wp
      h = if empty b (r,c) && hNeighbour b (r,c)
          then wordOnRow (updateSquare b wp) (r,c)
          else Nothing
      v = if empty b (r,c) && vNeighbour b (r,c)
          then wordOnCol (updateSquare b wp) (r,c)
          else Nothing in
  filter ((>1) . length) $ catMaybes [h, v] ++ additionalWords b wps

empty :: Board -> Pos -> Bool
empty b pos = isNothing (getSquare b pos)

hNeighbour :: Board -> Pos -> Bool
hNeighbour b (r,c) = isJust (getSquare b (r,c-1)) || isJust (getSquare b (r,c+1))

vNeighbour :: Board -> Pos -> Bool
vNeighbour b (r,c) = isJust (getSquare b (r-1,c)) || isJust (getSquare b (r+1,c))

wordFromSquare :: Board -> (Pos -> Pos) -> Pos -> WordPut
wordFromSquare b f pos = case getSquare b pos of
    Nothing -> []
    Just t  -> (pos, t) : wordFromSquare b f (f pos)

startOfWord :: Board -> (Pos -> Pos) -> Pos -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'

onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r < 15 && c >= 0 && c < 15

incRow :: Pos -> Pos
incRow (r,c) = (r+1,c)

incCol :: Pos -> Pos
incCol (r,c) = (r,c+1)

decRow :: Pos -> Pos
decRow (r,c) = (r-1,c)

decCol :: Pos -> Pos
decCol (r,c) = (r,c-1)

neighbours :: Pos -> [Pos]
neighbours (r,c) = filter (\(x,y) -> x >= 0 && x < 15 && y >= 0 && y < 15)
  [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

occupiedNeighbours :: Board -> Pos -> [Pos]
occupiedNeighbours b pos = filter (isJust . getSquare b) $ neighbours pos

wordOnRow :: Board -> Pos -> Maybe WordPut
wordOnRow b (r,c) = if isJust (getSquare b (r,c-1)) || isJust (getSquare b (r,c+1))
                    then Just (wordFromSquare b incCol (startOfWord b decCol (r,c)))
                    else Nothing

wordOnCol :: Board -> Pos -> Maybe WordPut
wordOnCol b (r,c) = if isJust (getSquare b (r-1,c)) || isJust (getSquare b (r+1,c))
                    then Just (wordFromSquare b incRow (startOfWord b decRow (r,c)))
                    else Nothing

mkWP :: String -> Pos -> Dir -> WordPut
mkWP w pos dir = let f = if dir == HZ then incCol else incRow in
  zip (iterate f pos) (map (\c -> fromJust (Map.lookup c charToLetterMap)) w) 

-- ========== Bonuses ============ --

data Bonus = Word Int | Letter Int

bonusSquaresList :: [((Int, Int), Bonus)] -- ^ ((Row, Column), Bonus)
bonusSquaresList = [((0, 0),   Word 3),    ((0, 3),   Letter 2)
                   , ((0, 7),   Word 3),   ((0, 11),  Letter 2)
                   , ((0, 14),  Word 3),   ((1, 1),   Word 2)
                   , ((1, 5),   Letter 3), ((1, 9),   Letter 3)
                   , ((1, 13),  Word 2),   ((2, 2),   Word 2)
                   , ((2, 6),   Letter 2), ((2, 8),   Letter 2)
                   , ((2, 12),  Word 2),   ((3, 0),   Letter 2)
                   , ((3, 3),   Word 2),   ((3, 7),   Letter 2)
                   , ((3, 11),  Word 2),   ((3, 14),  Letter 2)
                   , ((4, 4),   Word 2),   ((4, 10),  Word 2)
                   , ((5, 1),   Letter 3), ((5, 5),   Letter 3)
                   , ((5, 9),   Letter 3), ((5, 13),  Letter 3)
                   , ((6, 2),   Letter 2), ((6, 6),   Letter 2)
                   , ((6, 8),   Letter 2), ((6, 12),  Letter 2)
                   , ((7, 0),   Word 3),   ((7, 3),   Letter 2)
                   , ((7, 7),   Word 2),   ((7, 11),  Letter 2)
                   , ((7, 14),  Word 3),   ((8, 2),   Letter 2)
                   , ((8, 6),   Letter 2), ((8, 8),   Letter 2)
                   , ((8, 12),  Letter 2), ((9, 1),   Letter 3)
                   , ((9, 5),   Letter 3), ((9, 9),   Letter 3)
                   , ((9, 13),  Letter 3), ((10, 4),  Word 2)
                   , ((10, 10), Word 2),   ((11, 0),  Letter 2)
                   , ((11, 3),  Word 2),   ((11, 7),  Letter 2)
                   , ((11, 11), Word 2),   ((11, 14), Letter 2)
                   , ((12, 2),  Word 2),   ((12, 6),  Letter 2)
                   , ((12, 8),  Letter 2), ((12, 12), Word 2)
                   , ((13, 1),  Word 2),   ((13, 5),  Letter 3)
                   , ((13, 9),  Letter 3), ((13, 13), Word 2)
                   , ((14, 0),  Word 3),   ((14, 3),  Letter 2)
                   , ((14, 7),  Word 3),   ((14, 11), Letter 2)
                   , ((14, 14), Word 3)]

bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList
