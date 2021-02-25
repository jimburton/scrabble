module Scrabble.Game
  ( Game(..)
  , Turn(..)
  , newGame
  , newBoard
  , newBag
  , takeTurn
  , takeFromRack
  , fillRack
  , getPlayer
  , setPlayer
  , toggleTurn
  , takeMove )
  where

import           System.Random
import           Prelude hiding   ( words )

import Scrabble.Board.Board
  ( Board
  , Rack
  , WordPut
  , Player(..)
  , scoreWord
  , validateRack
  , validateMove
  , newBoard
  , updateBoard
  , empty
  , additionalWords )
import Scrabble.Board.Bag
  ( Bag
  , newBag
  , fillRack )
import Scrabble.Dict.Dict
  ( Dict
  , wordsInDict )

-- ============= Functions for playing the game =============== --

data Turn = P1 | P2 deriving (Show, Eq)

data Game = Game { board   :: Board
                 , bag     :: Bag
                 , player1 :: Player
                 , player2 :: Player
                 , turn    :: Turn
                 , gen     :: StdGen }

-- | Start a new game.
newGame :: String -- ^ Name of Player 1
        -> String -- ^ Name of Player 2
        -> StdGen -- ^ The random generator
        -> Game
newGame p1Name p2Name theGen = 
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
                , gen = gen'' } in
    g

-- | Take a turn in the game. Play a word onto the board then reset the current
--   player's rack and pass the turn to the next player.
takeTurn :: Dict    -- ^ The dictionary
         -> Game    -- ^ The game
         -> Bool    -- ^ Is first move
         -> WordPut -- ^ The word to play
         -> Either String Game
takeTurn d g fm wp =
  let theGen = gen g
      r      = rack (getPlayer g)
      theBag = bag g in
  case takeMove d g wp fm of
    Right g' -> let theRack = takeFromRack r wp
                    (filledRack, bag', gen') = fillRack theRack theBag theGen
                    p'   = (getPlayer g') { rack = filledRack }
                    g''  = setPlayer g' p'
                    g''' = toggleTurn g'' in
                  Right g''' { bag = bag', gen = gen' }
    Left e   -> Left e

-- | Take a move. Checks that this word is in the player's rack then calls the
--   move function.
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

-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. The word is validated as being in the dictionary.
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
                 Right g' {board = updateBoard b w}
      Left e -> Left e
    Left e -> Left e

-- | Play a sequence of letters onto a board, updating the score of the current player.
--   The word is NOT validated as being in the dictionary.
move' :: Game    -- ^ The game
      -> WordPut -- ^ The word to play
      -> Bool    -- ^ Is first move
      -> Either String Game
move' g w fm =
  let b   = board g
      p   = getPlayer g
      aw  = additionalWords b w
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      waw = map (map snd) (w:aw)
      sc  = sum $ map scoreWord sws in
  case validateMove b p w fm of
    Right _ -> let g' = setScore g sc in
                 Right g' {board = updateBoard b w}
    Left e -> Left e

-- | Update the current player in the game. 
setPlayer :: Game -> Player -> Game
setPlayer g p = if turn g == P1
                then g { player1 = p }
                else g { player2 = p }

-- | Take some letters from a rack.
takeFromRack :: Rack    -- ^ The rack to take from
             -> WordPut -- ^ The letters to take from the rack
             -> Rack
takeFromRack r = filter (not . (`elem` r)) . map snd 

-- | Toggle the turn in the game (between P1 and P2)
toggleTurn :: Game -- ^ The game in which to toggle the turn
           -> Game
toggleTurn g = g { turn = if turn g == P1 then P2 else P1 }

-- | Update the score of the current player in the game.
setScore :: Game -- ^ The game to be updated
         -> Int  -- ^ The new score of the current player
         -> Game
setScore g s = if turn g == P1
               then let s' = score (player1 g) in
                    g { player1 = (player1 g) {score = s' + s} }
               else let s' = score (player2 g) in
                    g { player2 = (player2 g) {score = s' + s} }

-- | Get the current player in the game.
getPlayer :: Game -> Player
getPlayer g = if turn g == P1 then player1 g else player2 g
  
