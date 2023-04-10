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
  , newBoard
  , scoreWords
  , additionalWords
  , makeWordPut
  , rackValue
  , adjacent
  , empty
  , getDirection
  , newTiles
  , freedomsFromWord
  , freeness
  , wordPutToWord
  , incCol
  , incRow
  , wordOnRow
  , wordOnCol )
  where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Array ( array )
import Data.Char (toUpper)
import Data.List (splitAt)
import Control.Lens ((^.),(.~),(&))
import Scrabble.Lang.Letter
  ( charToLetterMap
  , scoreLetter )
import Scrabble.Types
  ( Game
  , board
  , Board
  , Pos
  , WordPut
  , Bonus(..)
  , Dir(..)
  , Rack
  , Tile ) 
import Scrabble.Evaluator ( Evaluator )
import Scrabble.Board.Internal
  ( incCol
  , incRow
  , updateSquare
  , empty
  , wordOnRow 
  , wordOnCol
  , newTilesInMove
  , getDirection
  , newTiles
  , adjacent
  , freedomsFromWord
  , freeness
  , wordPutToWord )
import Scrabble.Board.Bonus (bonusMap)

-- * Boards

-- | A new, empty board.
newBoard :: Board
newBoard = array ((0,0),(14,14)) [((i,j), Nothing) | i <- [0..14], j <- [0..14]]

-- | Calculate the score of playing a word onto the board, including
--   bonuses and other words that may be created.
scoreWords :: Game      -- ^ The game.
           -> WordPut   -- ^ The word being played.
           -> [WordPut] -- ^ The additional words generated by playing the word.
           -> Evaluator Int -- ^ The score for these words.
scoreWords g w aw = do
  let b = g ^. board
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      fpb = if newTilesInMove b w == 7 then 50 else 0
  pure $ sum $ map (scoreWord fpb) sws

--  The Bool argument is whether any bonus should be applied to this
--  Pos and Tile, i.e. whether this is the first time it has been
--  counted.
scoreWord :: Int -- Starting bonus. This applies only for the seven letter word bonus.
          -> [(Pos, Tile, Bool)] -- (Position on board, tile, apply bonus).
          -> Int -- Score for the word.
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

-- | Make a @WordPut@ from a @Text@.
makeWordPut :: Text -- ^ The @Text@.
            -> Pos    -- ^ The starting position on the board.
            -> Dir    -- ^ The direction in which the word is to be placed.
            -> [Int]  -- ^ Indices of the letters which were blanks
            -> WordPut -- ^ The @WordPut@.
makeWordPut w pos dir is =
  let f  = if dir == HZ then incCol else incRow 
      wp = zip (iterate f pos)
           (map (\c -> let l = fromJust (Map.lookup (toUpper c) charToLetterMap) in 
                                             (l, scoreLetter l)) $ T.unpack w) in
    foldl zeroScore wp is 

-- Replace an element at a certain index in a list using a function.
replaceBy :: [a] -> Int -> (a -> a) -> [a]
replaceBy xs i f = case splitAt i xs of
   (before, x:after) -> before ++ f x : after
   _ -> xs

-- Set the score of a letter in a word to zero.
zeroScore :: WordPut -> Int -> WordPut
zeroScore xs i = replaceBy xs i (\(p,(l,_)) -> (p,(l,0)))

-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Evaluator Game
updateBoard w g = pure (g & board .~ foldl updateSquare (g ^. board) w)

-- | Get the value of the tiles in the rack.
rackValue :: Rack -> Int
rackValue = sum . map scoreLetter

