module Scrabble.Board.Board
  ( validateMove
  , validateRack
  , getSquare
  , updateSquare
  , newBoard
  , scoreLetter
  , bonusMap
  , scoreWord
  , empty
  , additionalWords
  , mkWP
  , newTilesInMove
  , replaceBy
  , replace
  , freedom
  , freedomsFromWord
  , newTiles
  , getDirection
  , incRow
  , incCol
  , decRow
  , decCol
  , canPlay )
  where

import Debug.Trace
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
import Data.List                 ( splitAt )
import Scrabble.Dict.Letter
  ( Letter
  , charToLetterMap
  , scoreLetter )
import Scrabble.Dict.Word
  ( wordToString )
import Scrabble.Types
  ( Board
  , Pos
  , WordPut
  , Player(..)
  , Bonus(..)
  , Dir(..)
  , Rack
  , PosTransform ) 
import Scrabble.Evaluator
  ( Evaluator(..)
  , evalBool )

newBoard :: Board
newBoard = listArray (0,14) $ replicate 15 (listArray (0,14) $ replicate 15 Nothing)

-- | The Bool argument is whether any bonus should be applied to this
--   Pos and Tile, i.e. whether this is the first time it has been
--   counted.
scoreWord :: Int -- ^ Starting bonus. This applies only for the seven letter word bonus.
          -> [(Pos, (Letter, Int), Bool)] -- ^ (Position on board, letter, apply bonus)
          -> Int
scoreWord fpb = scoreWord' 0 1 where
  scoreWord' s b [] = (s * b) + fpb
  scoreWord' s b ((pos,t,p):ws) =
    if not p then scoreWord' (snd t + s) b ws 
    else case Map.lookup pos bonusMap of
      Nothing -> scoreWord' (snd t + s) b ws
      Just b'  -> case b' of
                   (Word i)   -> scoreWord' (snd t + s) (i*b) ws
                   (Letter i) -> scoreWord' ((snd t * i) + s) b ws

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
{-

freedomFromRow :: Board -> Pos -> Letter -> Dir -> (Pos, Letter, (Int, Int))
freedomFromRow b (r,c) l dir = let f = if dir == HZ then decRow else decCol
                            g = if dir == HZ then incRow else incCol
                            h = if dir == HZ then fst else snd
                            ns = takeWhile (\p -> canPlay b p && canPlay b (f p))
                                 (iterate f (r,c))
                            n  = if null ns then 0 else h (last ns)
                            ss = takeWhile (\p -> canPlay b p && canPlay b (g p))
                                 (iterate g (r,c))
                            s  = if null ss then 0 else h (last ss) in
                          ((r,c),l,(r-n,s-r))

-}

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

-- | Find the additional words that are created by placing a word on the board.
additionalWords :: Board   -- ^ The board.
                -> WordPut -- ^ The new word.
                -> [WordPut]
additionalWords b w = additionalWords' w
  where additionalWords' []       = []
        additionalWords' (wp:wps) =
          let (r,c) = fst wp
              h = [wordOnCol (updateSquare b wp) (r,c) |
                    empty b (r,c) && not (all (`touches` w) (hNeighbours b (r,c)))]
              v = [wordOnRow (updateSquare b wp) (r,c) |
                    empty b (r,c) && not (all (`touches` w) (vNeighbours b (r,c)))] in
            h ++ v ++ additionalWords' wps

-- | Get direction of a word on the board. WordPuts must be at least two tiles
--   long.
getDirection :: WordPut -> Dir
getDirection w = let r1 = fst $ fst $ head w
                     r2 = fst $ fst $ head (tail w) in
                   if  r1<r2 then VT else HZ
  
-- | Is a square empty?
empty :: Board -> Pos -> Bool
empty b pos = isNothing (getSquare b pos)

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

-- | Make a WordPut from a string.
mkWP :: String -- ^ The string.
     -> Pos    -- ^ The starting position on the board.
     -> Dir    -- ^ The direction in which the word is to be placed.
     -> [Int]  -- ^ Indices of the letters which were blanks
     -> WordPut
mkWP w pos dir is = let f  = if dir == HZ then incCol else incRow 
                        wp = zip (iterate f pos)
                          (map (\c -> let l = fromJust (Map.lookup (toUpper c) charToLetterMap) in 
                                        (l, scoreLetter l)) w) in
                      foldl zeroScore wp is 

replace :: [a] -> Int -> a -> [a]
replace xs i e = replaceBy xs i (const e)

replaceBy :: [a] -> Int -> (a -> a) -> [a]
replaceBy xs i f = case splitAt i xs of
   (before, x:after) -> before ++ f x : after
   _ -> xs

zeroScore :: WordPut -> Int -> WordPut
zeroScore xs i = replaceBy xs i (\(p,(l,_)) -> (p,(l,0)))

-- ================= Validation ===============--

-- | Check that a move is valid: it touches at least one existing word (unless
--   it is the first move, in which case check that it touches the centre square),
--   it is in a straight and continuous line, and is made
--   up of letters that either in the rack or on the board.
validateMove :: Board    -- ^ The board
             -> Player   -- ^ The player making the move
             -> WordPut  -- ^ The word to play
             -> Bool     -- ^ Is first move
             -> Evaluator Bool
validateMove b p w fm =
  connects w b fm >>
  straight w >>
  (not fm || touches (7,7) w) `evalBool`
  "First move must touch centre square" >>
  all (\(pos,(t,_)) -> maybe (t `elem` rack p) ((==t) . fst) (getSquare b pos)) w `evalBool`
  ("Letters not in rack or not on board: " ++ formatWP w)

-- | Check that a word to be played is made of tiles that are either in the player's
--   rack or are already on the board.
validateRack :: Board
             -> Rack
             -> WordPut
             -> Evaluator Bool
validateRack b r w = someNewTiles b w >>
  all (\(pos,(t,_)) -> t `elem` r
           || (not (empty b pos) && (fst . fromJust . getSquare b) pos == t)) w
  `evalBool` ("Not all tiles in rack or on board: " ++ formatWP w)

formatWP :: WordPut -> String
formatWP w = wordToString (map (fst . snd) w) ++ ": " ++ show (fst (head w))
             ++ " " ++ show (getDirection w)

-- | Does the word touch the pos on the board?
touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

-- | New words must touch another (apart from the first one to be played)
connects :: WordPut -- ^ The word to play
         -> Board    -- ^ The board
         -> Bool     -- ^ Is first move
         -> Evaluator Bool
connects [] _ fm     = if fm then pure True else fail "Not touching any other tile"
connects (w:ws) b fm = let (pos,_) = w in
  if (not . all null) (occupiedNeighbours b pos)
  then pure True
  else connects ws b fm

-- | Words must contain at least two tiles and must be in a straight line on the board
straight :: WordPut -> Evaluator Bool
straight (w:x:xs) = let (r,_)  = fst w
                        (r',_) = fst x
                        ps     = map fst xs
                        sel    = if r == r'-1 then fst else snd
                        f      = \(x',y') -> sel x' == sel y' - 1 in 
                      (all f . zip ps $ tail ps) `evalBool` "Not in a straight line"
straight _         = fail "Too few letters"

-- | Check that a word to be played incudes some tiles that aren't on the board.
someNewTiles :: Board -> WordPut -> Evaluator Bool
someNewTiles b w = any (empty b . fst) w `evalBool`
  ("You didn't play any new tiles: " ++ formatWP w ++ show w)

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
                        
-- ========== Bonuses ============ --

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
