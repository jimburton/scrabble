module Main
  where

import  System.Random  ( getStdGen )
import  Scrabble.Game  ( Game(..)
                       , Turn(..)
                       , takeFromRack
                       , fillRack
                       , newBag
                       , newBoard
                       , getPlayer
                       , setPlayer
                       , toggleTurn
                       , takeMove
                       , mkWP )
import Scrabble.Board  ( Player(..)
                       , Board
                       , Dir(..) )
import Scrabble.Show   ( showGame
                       , showPlayer
                       , showTurn
                       , showBoard )
import Scrabble.Dict   ( Dict
                       , englishDictionary )

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
  d <- englishDictionary
  takeTurn d g True

takeTurn :: Dict -- ^ The dictionary
         -> Game -- ^ The game
         -> Bool -- ^ Is first move
         -> IO Game
takeTurn d g fm = do
  let r      = rack (getPlayer g)
      theBag = bag g
  printBoardAndTurn g
  [word,rowStr,colStr,dirStr] <- fmap words getLine
  let row = read rowStr :: Int
      col = read colStr :: Int
      dir = if dirStr == "H" then HZ else VT
      wp  = mkWP word (row,col) dir
      m   = takeMove d g wp fm 
  case m of
    Right g' -> do let theGen = gen g'
                       theRack = takeFromRack r wp
                       (filledRack, bag', gen') = fillRack theRack theBag theGen
                       p'   = (getPlayer g') { rack = filledRack }
                       g''  = setPlayer g' p'
                       g''' = toggleTurn g''
                   takeTurn d (g''' { bag = bag', gen = gen' }) False
    Left e   -> do putStrLn e
                   takeTurn d g False

printBoard :: Bool -> Board -> IO ()
printBoard printBonuses b = putStrLn $ showBoard printBonuses b

printGame :: Bool -> Board -> Player -> IO ()
printGame printBonuses b p = putStrLn $ showGame printBonuses b p

printPlayer :: Player -> IO ()
printPlayer p = putStrLn $ showPlayer p

printTurn :: Game -> IO ()
printTurn g = putStrLn $ showTurn g

printBoardAndTurn :: Game -> IO ()
printBoardAndTurn g = do printBoard True (board g)
                         printTurn g

main :: IO ()
main = putStrLn "Hello, world!"

