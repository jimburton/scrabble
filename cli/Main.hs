module Main
  where

import System.Random  ( getStdGen )
import System.Console.Haskeline
import Data.Foldable  ( forM_ )
import Data.Char      ( toUpper ) 
import Control.Monad.IO.Class ( liftIO )
import Scrabble.Types
  ( Game(..)
  , Dir(..)
  , Player(..)
  , Board )
import Scrabble.Game
  ( newGame
  , getPlayer
  , move
  , valWithRulesAndDict
  , valGameRules ) 
import Scrabble.Board.Board
  ( mkWP )
import Scrabble.Show
  ( showGame
  , showPlayer
  , showBoard )
import Scrabble.Dict.Dict
  ( englishDictionaryT )
import Scrabble.Evaluator ( Evaluator(..) )

startGame :: String -- ^ Name of Player 1
          -> String -- ^ Name of Player 2
          -> IO Game
startGame p1Name p2Name = do
  theGen <- getStdGen
  d      <- englishDictionaryT
  playGame (newGame p1Name p2Name theGen d)

playGame :: Game -> IO Game
playGame g = do
  printBoard True (board g) Nothing
  printPlayer $ player1 g
  printPlayer $ player2 g
  takeTurn g Nothing

takeTurn :: Game -- ^ The game
         -> Maybe String -- ^ Previous score as a string
         -> IO Game
takeTurn g msc = runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     liftIO $ printBoard True (board g) msc
     mLn <- getInputLine (showTurn g)
     case mLn of
       Nothing -> loop
       Just wdStr -> do
         let wds = words wdStr
         (wd,is) <- liftIO $ replaceBlanks (head wds)
         let [rowStr,colStr,dirStr] = tail $ words wdStr
             row = read rowStr :: Int
             col = read colStr :: Int
             dir = if map toUpper dirStr == "H" then HZ else VT
             wp  = mkWP wd (row,col) dir
         case move valWithRulesAndDict g wp >>= \(g',sc) -> do 
           let msc'  = Just (wd  ++ ": " ++ show sc)
           pure $ takeTurn g' msc' of
           (Ev (Left e))  -> do liftIO $ putStrLn e
                                liftIO $ takeTurn g $ Just (wd  ++ ": NO SCORE")
           (Ev (Right game)) -> liftIO game

replaceBlanks :: String
              -> IO (String, [Int]) -- ^ The unblanked string and the positions that were blanks 
replaceBlanks wd = if countElem '_' wd == 0
                   then return (wd,[])
                   else do ub <- unBlank wd
                           return (ub, indices '_' wd)
  where unBlank :: String -> IO String
        unBlank []       = return []
        unBlank ('_':us) = runInputT defaultSettings $ do
          mc <- getInputLine "Enter a letter for the blank:"
          case mc of
            (Just c) -> do rst <- liftIO $ unBlank us
                           return $ head c : rst
            Nothing  -> liftIO $ unBlank ('_':us)   
        unBlank (u:us)   = do rst <- unBlank us
                              return $ u : rst
  
printBoard :: Bool -> Board -> Maybe String -> IO ()
printBoard printBonuses b msc = do putStrLn $ showBoard printBonuses b
                                   forM_ msc putStrLn

printGame :: Bool -> Board -> Player -> IO ()
printGame printBonuses b p = putStrLn $ showGame printBonuses b p

printPlayer :: Player -> IO ()
printPlayer p = putStrLn $ showPlayer p

printTurn :: Game -> IO ()
printTurn g = putStrLn $ showTurn g

printBoardAndTurn :: Game -> Maybe String -> IO ()
printBoardAndTurn g msc = do printBoard True (board g) msc
                             printTurn g

showTurn :: Game -> String
showTurn g = let p = getPlayer g in
  showPlayer p ++ "Enter WORD ROW COL DIR[H/V]:\n"

indices :: Eq a => a -> [a] -> [Int]
indices x xs = map fst $ filter ((==x) . snd) (zip [0..14] xs)

countElem :: Eq a => a -> [a] -> Int
countElem _ []     = 0
countElem x (y:ys) = if x==y then 1 + countElem x ys else countElem x ys

main :: IO ()
main = putStrLn "Hello, world!"

