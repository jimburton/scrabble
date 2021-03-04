module Scrabble.Board.Board
  ( updateBoard 
  , newBoard
  , scoreLetter
  , bonusMap
  , scoreWords
  , additionalWords
  , mkWP
  , replaceBy
  , replace
  , empty
  , freedomsFromWord
  , getDirection
  , newTiles )
  where

import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe
  ( fromJust )
import Data.Array
import Data.Map
  ( Map )
import Data.Char
  ( toUpper )
import Data.List
  ( splitAt )
import Scrabble.Lang.Letter
  ( charToLetterMap
  , scoreLetter )
import Scrabble.Types
  ( Game(..)
  , Board
  , Pos
  , WordPut
  , Bonus(..)
  , Dir(..)
  , Letter ) 
import Scrabble.Evaluator
  ( Evaluator(..) )
import Scrabble.Board.Validation
  ( touches ) 
import Scrabble.Board.Internal
  ( incCol
  , incRow
  , updateSquare
  , empty
  , vNeighbours
  , hNeighbours
  , wordOnRow 
  , wordOnCol
  , newTilesInMove
  , freedomsFromWord
  , getDirection
  , newTiles )


newBoard :: Board
newBoard = listArray (0,14) $ replicate 15 (listArray (0,14) $ replicate 15 Nothing)

-- | Calculate the score of playing a word onto the board, including
--   bonuses and other words that may be created.
scoreWords :: Game -> WordPut -> [WordPut] -> Evaluator Int
scoreWords g w aw = do
  let b = board g
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      fpb = if newTilesInMove b w == 7 then 50 else 0
  pure $ sum $ map (scoreWord fpb) sws

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

-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Evaluator Game
updateBoard w g = pure g { board = foldl updateSquare (board g) w}
  
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
