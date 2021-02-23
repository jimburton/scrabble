module Scrabble.Game ( Game(..)
                     , Turn(..)
                     , newBoard
                     , newBag
                     , takeTurn
                     , takeFromRack
                     , fillRack
                     , getPlayer
                     , setPlayer
                     , toggleTurn
                     , takeMove
                     , mkWP )
  where

import           System.Random
import           Data.Maybe       ( isNothing
                                  , isJust
                                  , catMaybes
                                  , fromJust)
import           Data.Array
import           Prelude hiding   (words)
import qualified Prelude  as P    (words)
import           Data.Map         (Map)
import qualified Data.Map as Map
import           Control.Monad.ST 

import Scrabble.Board  ( Board
                       , Rack
                       , WordPut
                       , Pos
                       , Dir(..)
                       , Bonus(..)
                       , Player(..)
                       , charToLetterMap 
                       , scoreWord
                       , validateRack
                       , validateMove
                       , newBoard
                       , incCol
                       , incRow
                       , updateBoard
                       , empty
                       , additionalWords
                       , numTilesList )
import Scrabble.Dict   ( Dict
                       , Letter
                       , wordsInDict )

type Bag = [Letter]

data Turn = P1 | P2 deriving (Show, Eq)

data Game = Game { board   :: Board
                 , bag     :: Bag
                 , player1 :: Player
                 , player2 :: Player
                 , turn    :: Turn
                 , gen     :: StdGen }

instance Show Bonus where
  show (Word i)   = 'W' : show i
  show (Letter i) = 'L' : show i

newBag :: Bag
newBag = concatMap (\(l,n) -> replicate n l) numTilesList

newGame :: String -- ^ Name of Player 1
        -> String -- ^ Name of Player 2
        -> StdGen -- ^ The random generator
        -> Game
newGame p1Name p2Name gen = 
  let (rack1, bag1, gen') = fillRack [] newBag gen
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
                , gen = gen'' } in
    g

takeTurn :: Dict    -- ^ The dictionary
         -> Game    -- ^ The game
         -> Bool    -- ^ Is first move
         -> WordPut -- ^ The word to play
         -> StdGen  -- ^ The random generator
         -> Either String Game
takeTurn d g fm wp gen =
  let r      = rack (getPlayer g)
      theBag = bag g in
  case takeMove d g wp fm of
    Right g' -> let theRack = takeFromRack r wp
                    (filledRack, bag', gen') = fillRack theRack theBag gen
                    p'   = (getPlayer g') { rack = filledRack }
                    g''  = setPlayer g' p'
                    g''' = toggleTurn g'' in
                  Right g''' { bag = bag', gen = gen' }
    Left e   -> Left e

takeMove :: Dict    -- ^ The dictionary
         -> Game    -- ^ The game
         -> WordPut -- ^ The word to play
         -> Bool    -- ^ Is first move
         -> Either String Game
takeMove d g w fm = 
  let p = getPlayer g
      b = board g
      r = rack p in
  case validateRack b r w of
    Right _ -> move d g w fm
    Left e  -> Left e

setPlayer :: Game -> Player -> Game
setPlayer g p = if turn g == P1
                then g { player1 = p }
                else g { player2 = p }

takeFromRack :: Rack -> WordPut -> Rack
takeFromRack r = filter (not . (`elem` r)) . map snd 

toggleTurn :: Game -> Game
toggleTurn g = g { turn = if turn g == P1 then P2 else P1 }

mkWP :: String -> Pos -> Dir -> WordPut
mkWP w pos dir = let f = if dir == HZ then incCol else incRow in
  zip (iterate f pos) (map (\c -> fromJust (Map.lookup c charToLetterMap)) w) 

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
      waw = map (map snd) (w:aw)
      sc  = sum $ map scoreWord sws in
  case validateMove b p w fm of
    Right _ -> case wordsInDict d waw of
      Right _ -> let g' = setScore g sc in
                 Right g' {board = updateBoard w b}
      Left e -> Left e
    Left e -> Left e

setScore :: Game -- ^ The game to be updated
         -> Int  -- ^ The new score of the current player
         -> Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                    g { player1 = (player1 g) {score = s' + s} }
               else let s' = score (player2 g) in
                    g { player2 = (player2 g) {score = s' + s} }
  
getPlayer :: Game -> Player
getPlayer g = if turn g == P1 then player1 g else player2 g
  
