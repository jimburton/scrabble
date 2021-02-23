module Scrabble.Game
  where

import           System.Random
import           Data.Maybe       (isNothing
                                  , isJust
                                  , catMaybes
                                  , fromJust)
import           Data.List        (intercalate)
import           Data.Array
import           Prelude hiding   (words)
import qualified Prelude  as P    (words)
import           Data.Map         (Map)
import qualified Data.Map as Map
import           Control.Monad.ST 

import Scrabble.Types
import Scrabble.Dict

newBoard :: Board
newBoard = listArray (0,14) $ replicate 15 (listArray (0,14) $ replicate 15 Nothing)

newBag :: Bag
newBag = concatMap (\(c, n, m) -> replicate n (Tile c m)) tilesData

tileScore :: Tile -> Int
tileScore (Tile _ s) = s

tileChar :: Tile -> Char
tileChar (Tile c _) = c

startGame :: String -- ^ Name of Player 1
          -> String -- ^ Name of Player 2
          -> IO Game
startGame p1Name p2Name = do
  theGen <- getStdGen
  let (rack1, bag1, gen') = fillRack [] newBag theGen
      p1 = Player { name = p1Name
                  , rack = rack1
                  , score = 0 }
      (rack2, bag2, gen'') = fillRack [] bag1 gen'
      p2 = Player { name = p2Name
                  , rack = rack2
                  , score = 0 }
      g  = Game { board = newBoard
                , bag = bag2
                , player1 = p1
                , player2 = p2
                , turn = P1
                , gen = gen'' }
  playGame g

playGame :: Game -> IO Game
playGame g = do
  printBoard True $ board g
  printPlayer $ player1 g
  printPlayer $ player2 g
  takeTurn g True

takeTurn :: Game -- ^ The game
         -> Bool -- ^ Is first move
         -> IO Game
takeTurn g fm = do
  let r      = rack (getPlayer g)
      theBag = bag g
  printBoardAndTurn g
  [word,rowStr,colStr,dirStr] <- fmap P.words getLine
  let row = read rowStr :: Int
      col = read colStr :: Int
      dir = if dirStr == "H" then HZ else VT
      wp  = mkWP word (row,col) dir
  m <- takeMove g wp fm
  case m of
    Right g' -> do let theGen = gen g'
                       theRack = takeFromRack r wp
                       (filledRack, bag', gen') = fillRack theRack theBag theGen
                       p'   = (getPlayer g') { rack = filledRack }
                       g''  = setPlayer g' p'
                       g''' = toggleTurn g''
                   takeTurn g''' { bag = bag', gen = gen' } False
    Left e   -> do putStrLn e
                   takeTurn g False

setPlayer :: Game -> Player -> Game
setPlayer g p = if turn g == P1 then g { player1 = p } else g { player2 = p }

takeFromRack :: Rack -> WordPut -> Rack
takeFromRack r = filter (not . (`elem` r)) . map snd 

toggleTurn :: Game -> Game
toggleTurn g = g { turn = if turn g == P1 then P2 else P1 }

mkWP :: String -> Pos -> Dir -> WordPut
mkWP w pos dir = let f = if dir == HZ then incCol else incRow in
  zip (iterate f pos) (map (\c -> fromJust (Map.lookup c tilesMap)) w) 

fillRack :: Rack -> Bag -> StdGen -> (Rack, Bag, StdGen)
fillRack r = fillRack' (7 - length r) r
    where fillRack' 0 r' b' g' = (r', b', g')
          fillRack' _ r' [] g' = (r', [], g')
          fillRack' n r' b' g' =
            let (t, b'', g'') = getTile b' g' in
            fillRack' (n-1) (t:r') b'' g''             

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

takeMove :: Game    -- ^ The game
         -> WordPut -- ^ The word to play
         -> Bool    -- ^ Is first move
         -> IO (Either String Game)
takeMove g w fm = do
  d <- englishDictionary
  let p = getPlayer g
      b = board g
      r = rack p
  case validateRack b r w of
    Right _ -> return $ move d g w fm
    Left e -> return $ Left e

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
  
move :: Dict    -- ^ The dictionary
     -> Game    -- ^ The game
     -> WordPut -- ^ The word to play
     -> Bool    -- ^ Is first move
     -> Either String Game
move d g w fm =
  let b   = board g
      p   = getPlayer g
      aw  = additionalWords b w
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      sc  = sum $ map scoreWord sws in
  case validateMove b p w fm of
    Right _ -> case wordsInDict d (w:aw) of
      Right _ -> let g' = setScore g sc in
                 Right g' {board = updateBoard w b}
      Left e -> Left e
    Left e -> Left e

wordsInDict :: Dict
            -> [WordPut]
            -> Either String Bool
wordsInDict _ []     = Right True
wordsInDict d (w:ws) = let wStr = wordString w in
                       if inDict d (wordString w)
                       then wordsInDict d ws
                       else Left ("Not in dictionary: "++wStr) 

setScore :: Game
         -> Int
         -> Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                    g { player1 = (player1 g) {score = s' + s} }
               else let s' = score (player2 g) in
                    g { player2 = (player2 g) {score = s' + s} }
  
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

validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
validateMove b p w fm = case connects w b fm of
  Right _ -> case straight w of
               Right _ -> if all (\(pos,t) -> case getSquare b pos of
                                     Just (Tile c _) -> c == tileChar t
                                     Nothing         -> t `elem` rack p) w
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
                         else Left "Not in a straight lin"
                    else if all (\(x',y') -> snd x' == snd y' - 1) . zip ps $ tail ps
                         then Right True
                         else Left "Not in a straight line"
straight _         = Left "Too few letters"

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
  rackLine   = let strs = map show (rack p) in
                 intercalate ", " (take 5 strs) ++ "\n"
                 ++ intercalate ", " (drop 5 strs) ++ "\n"
  bottom     = line '*'

printPlayer :: Player -> IO ()
printPlayer p = putStrLn $ showPlayer p

showTurn :: Game -> String
showTurn g = let p = getPlayer g in
  showPlayer p ++ "Enter WORD ROW COL DIR[H/V]:\n"

printTurn :: Game -> IO ()
printTurn g = putStrLn $ showTurn g

printBoardAndTurn :: Game -> IO ()
printBoardAndTurn g = do printBoard True (board g)
                         printTurn g

wordString :: WordPut -> String
wordString = map (tileChar . snd)

{-- Data for the board. --}

tilesData :: [(Char, Int, Int)]
tilesData = [ ('A', 9,  1) -- (Letter, NumTiles, Score)
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

tilesMap :: Map Char Tile
tilesMap = Map.fromList (map (\(c,_,s) -> (c, Tile c s)) tilesData)

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
