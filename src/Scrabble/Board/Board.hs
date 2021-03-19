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
  , newTiles
  , rackValue )
  where

import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe
  ( fromJust
  , catMaybes )
import Data.Array
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
  , Letter
  , Rack ) 
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
import Scrabble.Board.Bonus
  ( bonusMap )

newBoard :: Board
newBoard = array ((0,0),(14,14)) [((i,j), Nothing) | i <- [0..14], j <- [0..14]]

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
                   W2 -> scoreWord' (snd t + s) (2*b) ws
                   W3 -> scoreWord' (snd t + s) (3*b) ws
                   L2 -> scoreWord' ((snd t * 2) + s) b ws
                   L3 -> scoreWord' ((snd t * 3) + s) b ws

-- | Find the additional words that are created by placing a word on the board.
additionalWords :: Game   -- ^ The game.
                -> WordPut -- ^ The new word.
                -> Evaluator [WordPut]
additionalWords g w = updateBoard w g >>= \g' -> do
  let b      = board g'
      oppDir = if getDirection w == HZ then VT else HZ
      ps     = map fst w
      mWds   = if oppDir == HZ then map (wordOnRow b) ps else map (wordOnCol b) ps
  pure $ catMaybes mWds

{--- | Find the additional words that are created by placing a word on the board.
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
-}
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

-- | Replace an element at a certain index in a list.
replace :: [a] -> Int -> a -> [a]
replace xs i e = replaceBy xs i (const e)

-- | Replace an element at a certain index in a list using a function.
replaceBy :: [a] -> Int -> (a -> a) -> [a]
replaceBy xs i f = case splitAt i xs of
   (before, x:after) -> before ++ f x : after
   _ -> xs

-- | Set the croe of a letter in a word to zero.
zeroScore :: WordPut -> Int -> WordPut
zeroScore xs i = replaceBy xs i (\(p,(l,_)) -> (p,(l,0)))

-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Evaluator Game
updateBoard w g = pure g { board = foldl updateSquare (board g) w}

-- | Get the value of the tiles in the rack.
rackValue :: Rack -> Int
rackValue = sum . map scoreLetter

