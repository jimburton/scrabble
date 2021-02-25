module Main
  where

import  System.Random  ( getStdGen )
import  Scrabble.Game  ( Game(..)
                       , Turn(..)
                       , newGame
                       , takeFromRack
                       , fillRack
                       , newBag
                       , newBoard
                       , getPlayer
                       , setPlayer
                       , toggleTurn
                       , takeMove )
import Scrabble.Board.Board  ( Player(..)
                       , Board
                       , Dir(..)
                       , mkWP )
import Scrabble.Show   ( showGame
                       , showPlayer
                       , showBoard )
import Scrabble.Dict.Dict   ( Dict
                            , englishDictionary )

startGame :: String -- ^ Name of Player 1
          -> String -- ^ Name of Player 2
          -> IO Game
startGame p1Name p2Name = do
  theGen <- getStdGen
  playGame (newGame p1Name p2Name theGen)

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


showTurn :: Game -> String
showTurn g = let p = getPlayer g in
  showPlayer p ++ "Enter WORD ROW COL DIR[H/V]:\n"


main :: IO ()
main = putStrLn "Hello, world!"

