module Scrabble.Game
  where

import System.Random
import Data.Maybe (isNothing
                  , isJust
                  , catMaybes)
import Data.List (intercalate)
import Data.Array
import Prelude hiding (words)

import Scrabble.Types
import Scrabble.Dict

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
                  where fillRack' 0 r' b' g = (r', b', g)
                        fillRack' _ r' [] g = (r', [], g)
                        fillRack' n r' b' g = let (t, b'', g') = getTile b' g in
                                                fillRack' (n-1) (t:r') b'' g'             

getTile :: RandomGen g => Bag -> g -> (Tile, Bag, g)
getTile b g = let (i, g') = randomR (0, length b -1) g
                  t = b !! i
                  b' = take i b ++ drop (i+1) b in
              (t, b', g')

-- | The Bool argument is whether any bonus should be applied to this
--   Pos and Tile, i.e. whether this is the first time it has been
--   counted.
scoreWord :: [(Pos, Tile, Bool)] -> Int
scoreWord = scoreWord' 0 1 where
  scoreWord' s b [] = s * b
  scoreWord' s b ((pos,t,p):ws) =
    if not p then scoreWord' (tileScore t + s) b ws
    else case lookup pos bonusSquares of
      Nothing -> scoreWord' (tileScore t + s) b ws
      Just b'  -> case b' of
                   (Word i)   -> scoreWord' (tileScore t + s) (i*b) ws
                   (Letter i) -> scoreWord' ((tileScore t * i) + s) b ws

takeMove :: Game -> WordPut -> IO Bool
takeMove g w = do d <- englishDictionary
                  let p = getPlayer g
                      r = rack p
                  if validateRack r w
                  then return True
                  else return False

validateRack :: Rack -> WordPut -> Bool
validateRack r = all ((`elem` r) . snd)

move :: Dict -> Game -> WordPut -> Either String Game
move d g w =
  let b   = board g
      p   = getPlayer g
      aw  = additionalWords b w
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      sc  = sum $ map scoreWord sws in
  if validateMove b p w && all (inDict d . wordString) (w:aw)
  then let g' = setScore g sc in
       Right g' {board = updateBoard w b}
  else Left "Can't play this word"

setScore :: Game -> Int -> Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                    g {player1 = (player1 g) {score = s' + s}}
               else let s' = score (player2 g) in
                    g {player2 = (player2 g) {score = s' + s}}
  
getPlayer :: Game -> Player
getPlayer g = if turn g == P1 then player1 g else player2 g
  
inDict :: Dict -> String -> Bool
inDict d str = case wordFromString str of
                 Nothing -> False
                 (Just w) -> dictContainsWord d w

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

{-
Validation
-}

validateMove :: Board -> Player -> WordPut -> Bool
validateMove b p w = connects w b
  && straight w
  && all (\(pos,t) -> case getSquare b pos of
                        Just (Tile c _) -> c == tileChar t
                        Nothing         -> t `elem` rack p) w


connects :: WordPut -> Board -> Bool
connects [] _     = True
connects (w:ws) b = let (pos,_) = w in
  (not . all null) (occupiedNeighbours b pos) || connects ws b

-- | WordPuts must have at least two elements 
straight :: WordPut -> Bool
straight (w:x:xs) = let (r,_)  = fst w
                        (r',_) = fst x
                        ps     = map fst xs in 
                    if r == r' - 1
                    then all (\(x',y') -> fst x' == fst y' - 1) . zip ps $ tail ps
                    else all (\(x',y') -> snd x' == snd y' - 1) . zip ps $ tail ps
straight _         = False

getSquare :: Board -> Pos -> Maybe Tile
getSquare b (r,c) = if onBoard (r,c)
                    then (b ! r) ! c
                    else Nothing

updateSquare :: Board -> (Pos, Tile) -> Board
updateSquare b ((r,c),t) = let row = (b ! r) // [(c, Just t)] in
                           b // [(r, row)]

updateBoard :: WordPut -> Board -> Board
updateBoard ws b = foldl updateSquare b ws

showBoard :: Bool -> Board -> String
showBoard printBonuses b = topNumbers ++ top ++ showRows ++ bottom where
  showRows      = intercalate "\n" (zipWith showRow [0..14] (elems b)) ++ "\n"
  showRow     i r = showI i ++ "|" ++ concat (zipWith (showSquare i) [0..14] (elems r)) 
  showSquare i c s = case s of
                       Nothing -> if printBonuses
                                  then case lookup (i,c) bonusSquares of
                                         Nothing -> "  |"
                                         Just b' -> show b' ++ "|"
                                  else "  |"
                       Just t -> [' ', tileChar t, '|']
  topNumbers    = "  |" ++ concatMap (\i -> showI i ++ "|") [0..14] ++ "\n"
  showI i       = if i < 10 then " " ++ show i else show i
  top           = line '-'
  bottom        = line '-'
  line        c = replicate 48 c ++ "\n"

printBoard :: Bool -> Board -> IO ()
printBoard printBonuses b = putStrLn $ showBoard printBonuses b

showGame :: Bool -> Board -> Player -> String
showGame printBonuses b p = showBoard printBonuses b ++ showPlayer p

printGame :: Bool -> Board -> Player -> IO ()
printGame printBonuses b p = putStrLn $ showGame printBonuses b p

showPlayer :: Player -> String
showPlayer p = top ++ playerLine ++ rackLine ++ bottom where
  line     c = replicate 46 c ++ "\n"
  top        = "\n" ++ line '*'
  playerLine = name p ++ " (" ++ show (score p) ++ ")\n"
  rackLine   = intercalate ", " (map show (rack p)) ++ "\n"
  bottom     = line '*'

wordString :: WordPut -> String
wordString = map (tileChar . snd)

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
