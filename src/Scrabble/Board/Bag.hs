module Scrabble.Board.Bag
  ( Bag
  , newBag
  , fillRack )
  where

import System.Random         ( RandomGen
                             , StdGen
                             , randomR )
import Scrabble.Dict.Letter  ( Letter )
import Scrabble.Board.Board  ( numTilesList )
import Scrabble.Board.Rack   ( Rack )

type Bag = [Letter]

newBag :: Bag
newBag = concatMap (\(l,n) -> replicate n l) numTilesList

fillRack :: Rack -> Bag -> StdGen -> (Rack, Bag, StdGen)
fillRack r = fillRack' (7 - length r) r
    where fillRack' 0 r' b' g' = (r', b', g')
          fillRack' _ r' [] g' = (r', [], g')
          fillRack' n r' b' g' =
            let (t, b'', g'') = getTile b' g' in
            fillRack' (n-1) (t:r') b'' g''             

getTile :: RandomGen g => Bag -> g -> (Letter, Bag, g)
getTile b g = let (i, g') = randomR (0, length b -1) g
                  t = b !! i
                  b' = take i b ++ drop (i+1) b in
              (t, b', g')

