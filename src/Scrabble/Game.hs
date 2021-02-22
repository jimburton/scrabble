module Scrabble.Game
  where

import System.Random
import Data.Maybe (isNothing
                  , isJust
                  , catMaybes)
import Data.List (intercalate)
import Data.Array

import Scrabble.Types

newBoard :: Board
newBoard = listArray (0,14) $ replicate 15 (listArray (0,14) $ replicate 15 Nothing)

newBag :: Bag
newBag = concatMap (\(c, n, m) -> replicate n (Tile c m)) tiles

tileScore :: Tile -> Int
tileScore (Tile _ s) = s

tileChar :: Tile -> Char
tileChar (Tile c _) = c

fillRack :: Rack -> Bag -> IO (Rack, Bag, StdGen)
fillRack r b = do fillRack' (7 - length r) r b <$> getStdGen 
                  where fillRack' 0 r b g = (r, b, g)
                        fillRack' n r [] g = (r, [], g)
                        fillRack' n r b g = let (t, b', g') = getTile b g in
                                                fillRack' (n-1) (t:r) b' g'             

getTile :: RandomGen g => Bag -> g -> (Tile, Bag, g)
getTile b g = let (i, g') = randomR (0, length b -1) g
                  t = b !! i
                  b' = take i b ++ drop (i+1) b in
              (t, b', g')

scoreWord :: WordPut -> Int
scoreWord = scoreWord' 0 1 where
  scoreWord' s b [] = s * b
  scoreWord' s b ((pos,t):ws) =
    case lookup pos bonusSquares of
      Nothing -> scoreWord' (tileScore t + s) b ws
      Just b'  -> case b' of
                   (Word i)   -> scoreWord' (tileScore t + s) (i*b) ws
                   (Letter i) -> scoreWord' ((tileScore t * i) + s) b ws

scorePlayer :: Player -> Int
scorePlayer (Player _ ws) = sum (map scoreWord ws)
  

move :: Board -> Player -> WordPut -> Either String (Board, Player)
move b (Player r ws) w = if legalMove (Player r ws) w b
                         then Right (updateBoard w b, Player r ws)
                         else Left "Can't play this word"

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

legalMove :: Player -> WordPut -> Board -> Bool
legalMove p w b = connects w b && all (\(pos,t) -> case getSquare b pos of
                                          Just (Tile c _) -> c == tileChar t
                                          Nothing -> t `elem` rack p) w

connects :: WordPut -> Board -> Bool
connects [] _     = True
connects (w:ws) b = let (pos,_) = w in
  any (isJust . getSquare b) (neighbours pos)
      
getSquare :: Board -> Pos -> Maybe Tile
getSquare b (r,c) = if onBoard (r,c)
                    then (b ! r) ! c
                    else Nothing

updateRow :: Int -> Tile -> [Maybe Tile] -> [Maybe Tile]
updateRow _ _ []     = []
updateRow 0 m (t:ts) = Just m :ts
updateRow n m (t:ts) = t : updateRow (n-1) m ts

updateSquare :: Board -> (Pos, Tile) -> Board
updateSquare b ((r,c),t) = let row = (b ! r) // [(c, Just t)] in
                           b // [(r, row)]

updateBoard :: WordPut -> Board -> Board
updateBoard ws b = foldl updateSquare b ws

showBoard :: Bool -> Board -> String
showBoard printBonuses b = top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (zipWith showRow [0..14] (elems b)) ++ "\n"
  showRow     i r = "|" ++ concat (zipWith (showSquare i) [0..14] (elems r)) 
  showSquare i c s = case s of
                       Nothing -> if printBonuses
                                  then case lookup (i,c) bonusSquares of
                                         Nothing -> "  |"
                                         Just b  -> show b ++ "|"
                                  else "  |"
                       Just t -> [' ', tileChar t, '|']
  top           = line '_'
  bottom        = line '-'
  line        c = replicate 46 c ++ "\n"

{-- Data for the board. --}

tiles :: [(Char, Int, Int)]
tiles = [ ('A', 9,  1) -- (Letter, NumTiles, Score)
        , ('B', 2,  3)
        , ('C', 2,  3)
        , ('D', 4,  2)
        , ('E', 12, 1)
        , ('F', 2,  4)
        , ('G', 3,  2)
        , ('H', 2,  4)
        , ('I', 9,  1)
        , ('J', 1,  8)
        , ('K', 1,  5)
        , ('L', 4,  1)
        , ('M', 2,  3)
        , ('N', 6,  1)
        , ('O', 8,  1)
        , ('P', 2,  3)
        , ('Q', 1,  10)
        , ('R', 6,  1)
        , ('S', 4,  1)
        , ('T', 6,  1)
        , ('U', 4,  1)
        , ('V', 2,  4)
        , ('W', 2,  4)
        , ('X', 1,  8)
        , ('Y', 2,  4)
        , ('Z', 1,  10)]

bonusSquares :: [((Int, Int), Bonus)] -- ((Row, Column), Bonus)
bonusSquares = [((0, 0),   Word 3)
               , ((0, 3),   Letter 2)
               , ((0, 7),   Word 3)
               , ((0, 11),  Letter 2)
               , ((0, 14),  Word 3)
               , ((1, 1),   Word 2)
               , ((1, 5),   Letter 3)
               , ((1, 9),   Letter 3)
               , ((1, 13),  Word 2)
               , ((2, 2),   Word 2)
               , ((2, 6),   Letter 2)
               , ((2, 8),   Letter 2)
               , ((2, 12),  Word 2)
               , ((3, 0),   Letter 2)
               , ((3, 3),   Word 2)
               , ((3, 7),   Letter 2)
               , ((3, 11),  Word 2)
               , ((3, 14),  Letter 2)
               , ((4, 4),   Word 2)
               , ((4, 10),  Word 2)
               , ((5, 1),   Letter 3)
               , ((5, 5),   Letter 3)
               , ((5, 9),   Letter 3)
               , ((5, 13),  Letter 3)
               , ((6, 2),   Letter 2)
               , ((6, 6),   Letter 2)
               , ((6, 8),   Letter 2)
               , ((6, 12),  Letter 2)
               , ((7, 0),   Word 3)
               , ((7, 3),   Letter 2)
               , ((7, 7),   Word 2)
               , ((7, 11),  Letter 2)
               , ((7, 14),  Word 3)
               , ((8, 2),   Letter 2)
               , ((8, 6),   Letter 2)
               , ((8, 8),   Letter 2)
               , ((8, 12),  Letter 2)
               , ((9, 1),   Letter 3)
               , ((9, 5),   Letter 3)
               , ((9, 9),   Letter 3)
               , ((9, 13),  Letter 3)
               , ((10, 4),  Word 2)
               , ((10, 10), Word 2)
               , ((11, 0),  Letter 2)
               , ((11, 3),  Word 2)
               , ((11, 7),  Letter 2)
               , ((11, 11), Word 2)
               , ((11, 14), Letter 2)
               , ((12, 2),  Word 2)
               , ((12, 6),  Letter 2)
               , ((12, 8),  Letter 2)
               , ((12, 12), Word 2)
               , ((13, 1),  Word 2)
               , ((13, 5),  Letter 3)
               , ((13, 9),  Letter 3)
               , ((13, 13), Word 2)
               , ((14, 0),  Word 3)
               , ((14, 3),  Letter 2)
               , ((14, 7),  Word 3)
               , ((14, 11), Letter 2)
               , ((14, 14), Word 3)]
