{-|
Module      : Scrabble.Board.Bag
Description : A bag of Scrabble tiles.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

A bag of tiles used in a game of scrabble.
-}
module Scrabble.Board.Bag
  ( newBag
  , fillRack
  , takeFromRack )
  where

import Data.List (delete)
import Prelude hiding (Word)
import System.Random
  ( StdGen
  , randomR )
import Scrabble.Types
  ( Bag
  , Rack
  , Word
  , Letter(..) )
import Scrabble.Evaluator (Evaluator)

-- ============ Functions relating to a bag of tiles ============= --

-- The number of each tile that is in a new bag.
numTilesList :: [(Letter,Int)]
numTilesList = [
  (A, 9), (B, 2), (C, 2), (D, 4), (E, 12), (F, 2), (G, 3),
  (H, 2), (I, 9), (J, 1), (K, 1), (L, 4), (M, 2), (N, 6),
  (O, 8), (P, 2), (Q, 1), (R, 6), (S, 4), (T, 6), (U, 4),
  (V, 2), (W, 2), (X, 1), (Y, 2), (Z, 1), (Blank, 2) ]

-- | A new bag containing a full set of tiles.
newBag :: Bag
newBag = concatMap (\(l,n) -> replicate n l) numTilesList

-- | Refill a rack with tiles picked randomly from the bag. Returns the filled
--   rack, the new bag and the updated random generator.
fillRack :: Rack   -- ^ The rack to fill 
         -> Bag    -- ^ The bag to pick from.
         -> StdGen -- ^ The random generator.
         -> Evaluator (Rack, Bag, StdGen) -- ^ The filled rack, the updated bag and the updated StdGen.
fillRack r b g = pure $ fillRack' (7 - length r) r b g
  where fillRack' 0 r' b' g' = (r', b', g')
        fillRack' _ r' [] g' = (r', [], g')
        fillRack' n r' b' g' =
          let (t, b'', g'') = getTile b' g' in
            fillRack' (n-1) (t:r') b'' g''

-- | Take some letters from a rack.
takeFromRack :: Rack -- ^ The rack to take from
             -> Word -- ^ The letters to take from the rack
             -> Evaluator Rack -- ^ The updated rack.
takeFromRack r = pure . deleteAll r

-- Delete the first occurence of each element in the second list from the first list.
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll = foldl (flip delete)

-- Get a single tile from a bag.
getTile :: Bag    -- The bag to take the tile from.
        -> StdGen -- The random generator.
        -> (Letter, Bag, StdGen) -- A letter from the bag, the updated bag and the updated StdGen.
getTile b g = let (i, g') = randomR (0, length b -1) g
                  t = b !! i
                  b' = take i b ++ drop (i+1) b in
              (t, b', g')
