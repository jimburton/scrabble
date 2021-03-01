module Scrabble.Board.Board
  ( Board
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
  , incCol
  , incRow
  , empty
  , additionalWords
  , mkWP
  , newTilesInMove )
  where

import qualified Data.Map as Map
import Data.Maybe
  ( fromJust
  , isNothing
  , catMaybes
  , isJust
  , mapMaybe )
import Data.Array
import Data.Map                  ( Map )
import Data.Char                 ( toUpper )
import Scrabble.Dict.Letter
  ( Letter(..)
  , toChar
  , charToLetterMap
  , scoreLetter )
import Scrabble.Board.Rack       ( Rack )

type Board = Array Int (Array Int (Maybe Letter))

data Player = Player { name :: String
                     , rack :: Rack
                     , score :: Int
                     } deriving (Show, Eq)

newBoard :: Board
newBoard = listArray (0,14) $ replicate 15 (listArray (0,14) $ replicate 15 Nothing)

type Pos = (Int, Int)

type WordPut = [(Pos, Letter)]

data Dir = HZ | VT deriving (Show, Read, Eq)

-- | The Bool argument is whether any bonus should be applied to this
--   Pos and Tile, i.e. whether this is the first time it has been
--   counted.
scoreWord :: Int -- ^ Starting bonus. This applies only for the seven letter word bonus.
          -> [(Pos, Letter, Bool)] -- ^ (Position on board, letter, apply bonus)
          -> Int
scoreWord fpb = scoreWord' fpb 1 where
  scoreWord' s b [] = s * b
  scoreWord' s b ((pos,t,p):ws) =
    if not p then scoreWord' (scoreLetter t + s) b ws 
    else case Map.lookup pos bonusMap of
      Nothing -> scoreWord' (scoreLetter t + s) b ws
      Just b'  -> case b' of
                   (Word i)   -> scoreWord' (scoreLetter t + s) (i*b) ws
                   (Letter i) -> scoreWord' ((scoreLetter t * i) + s) b ws

-- | How many new tiles are being played in a move?
newTilesInMove :: Board -> WordPut -> Int
newTilesInMove b = length . mapMaybe (getSquare b . fst) 

-- ================= Validation ===============--

-- | Check that a move is valid: it touches at least one existing word (unless
--   it is the first move, in which case check that it touches the centre square),
--   it is in a straight and continuous line, and is made
--   up of letters that either in the rack or on the board.
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

-- | Does the word touch the pos on the board?
touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

-- | New words must touch another (apart from the first one to be played)
connects :: WordPut -- ^ The word to play
         -> Board   -- ^ The board
         -> Bool    -- ^ Is first move
         -> Either String Bool
connects [] _ fm     = if fm then Right True else Left "Not touching any other tile"
connects (w:ws) b fm = let (pos,_) = w in
  if (not . all null) (occupiedNeighbours b pos)
  then Right True
  else connects ws b fm

-- | Words must contain at least two tiles and must be in a straight line on the board
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

-- | Check that a word to be played is made of tiles that are either in the player's
--   rack or are already on the board.
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

-- | Check that a word to be played incudes some tiles that aren't on the board.
someNewTiles :: Board
             -> WordPut
             -> Either String Bool
someNewTiles b w = if any (empty b . fst) w
                   then Right True
                   else Left "You didn't play any new tiles"

-- ==================== Manipulating and querying the board =================--

-- | Retrieve a position on the board.
getSquare :: Board -> Pos -> Maybe Letter
getSquare b (r,c) = if onBoard (r,c)
                    then (b ! r) ! c
                    else Nothing

-- | Place a tile onto the board.
updateSquare :: Board -> (Pos, Letter) -> Board
updateSquare b ((r,c),l) = let row = (b ! r) // [(c, Just l)] in
                             b // [(r, row)]


-- | Place a word onto the board.
updateBoard :: Board -> WordPut -> Board
updateBoard = foldl updateSquare

-- | Stringify a set of tiles on the board.
wordString :: WordPut -> String
wordString = map (toChar . snd)

-- | Find the additional words that are created by placing a word on the board.
additionalWords :: Board   -- ^ The board.
                -> WordPut -- ^ The new word.
                -> [WordPut]
additionalWords b w = additionalWords' w
  where additionalWords' []       = []
        additionalWords' (wp:wps) =
          let (r,c) = fst wp
              h = if empty b (r,c) && not (all (`touches` w) (hNeighbours b (r,c)))
                  then wordOnRow (updateSquare b wp) (r,c)
                  else Nothing
              v = if empty b (r,c) && not (all (`touches` w) (vNeighbours b (r,c)))
                  then wordOnCol (updateSquare b wp) (r,c)
                  else Nothing in
            filter ((>1) . length) $ catMaybes [h, v] ++ additionalWords' wps

-- | Get direction of a word on the board. WordPuts must be at least two tiles
--   long.
-- getDirection :: WordPut -> Dir
-- getDirection w = let r1 = fst $ head w
--                     r2 = fst $ head (tail w) in
--                   if  r1<r2 then HZ else VT
  
-- | Is a square empty?
empty :: Board -> Pos -> Bool
empty b pos = isNothing (getSquare b pos)

-- | The occupied horizonal neighbours of a position on the board.
hNeighbours :: Board -> Pos -> [Pos]
hNeighbours b (r,c) = let west = [(r-1,c) | isJust (getSquare b (r-1,c))]
                          east = [(r+1,c) | isJust (getSquare b (r+1,c))] in
                        east ++ west

-- | The occupied vertical neighbours of a position on the board.
vNeighbours :: Board -> Pos -> [Pos]
vNeighbours b (r,c) = let north = [(r,c-1) | isJust (getSquare b (r,c-1))]
                          south = [(r,c+1) | isJust (getSquare b (r,c+1))] in
                        north ++ south

-- | Is there an occupied horizontal neighbour of a position on the board.
hNeighbour :: Board -> Pos -> Bool
hNeighbour b (r,c) = isJust (getSquare b (r-1,c)) || isJust (getSquare b (r+1,c))

-- | Is there an occupied vertical neighbour of a position on the board.
vNeighbour :: Board -> Pos -> Bool
vNeighbour b (r,c) = isJust (getSquare b (r,c-1)) || isJust (getSquare b (r,c+1))

-- | TODO Do I need this?
wordFromSquare :: Board
               -> (Pos -> Pos) -- ^ The function that moves to the start of the word (up rows or left along columns)
               -> Pos
               -> WordPut
wordFromSquare b f pos = case getSquare b pos of
    Nothing -> []
    Just t  -> (pos, t) : wordFromSquare b f (f pos)

-- | Find the starting position of a word that crosses a position on the board.
startOfWord :: Board        -- The board.
            -> (Pos -> Pos) -- ^ The function that moves to the start of the word (up rows or left along columns)
            -> Pos          -- ^ The position
            -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'

-- | Is a position on the board?
onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r < 15 && c >= 0 && c < 15

-- | Increment the row within a position.
incRow :: Pos -> Pos
incRow (r,c) = (r+1,c)

-- | Increment the column within a position.
incCol :: Pos -> Pos
incCol (r,c) = (r,c+1)

-- | Decrement the row within a position.
decRow :: Pos -> Pos
decRow (r,c) = (r-1,c)

-- | Decrement the column within a position.
decCol :: Pos -> Pos
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
          -> Maybe WordPut
wordOnRow b (r,c) = if isJust (getSquare b (r,c-1)) || isJust (getSquare b (r,c+1))
                    then Just (wordFromSquare b incCol (startOfWord b decCol (r,c)))
                    else Nothing

-- | Retrieve the word that crosses a position on the board vertically.
wordOnCol :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> Maybe WordPut
wordOnCol b (r,c) = if isJust (getSquare b (r-1,c)) || isJust (getSquare b (r+1,c))
                    then Just (wordFromSquare b incRow (startOfWord b decRow (r,c)))
                    else Nothing

-- | Make a WordPut from a string.
mkWP :: String -- ^ The string.
     -> Pos    -- ^ The starting position on the board.
     -> Dir    -- ^ The direction in which the word is to be placed.
     -> WordPut
mkWP w pos dir = let f = if dir == HZ then incCol else incRow in
  zip (iterate f pos) (map (\c -> fromJust (Map.lookup (toUpper c) charToLetterMap)) w) 

-- ========== Bonuses ============ --

data Bonus = Word Int | Letter Int -- ^ The datatype of bonuses

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

-- | Data for the bonus map.
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

-- | Map containing the bonus squares on the board.
bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList
