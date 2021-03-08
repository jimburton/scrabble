{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import Debug.Trace
import System.Random  ( getStdGen )
import System.Console.Haskeline
import Data.Foldable  ( forM_ )
import Data.Char      ( toUpper ) 
import Control.Monad.IO.Class ( liftIO )
import Data.Maybe ( fromJust )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Scrabble.Types
  ( Game(..) 
  , Dir(..)
  , Player(..)
  , Board )
import Scrabble.Game.Game
  ( newGame
  , getPlayer
  , move
  , swap
  , pass )
import Scrabble.Game.Validation
  ( valGameRulesAndDict
  , valGameRules )
import Scrabble.Game.AI
  ( newGame1P
  , moveAI )
import Scrabble.Board.Board
  ( mkWP )
import Scrabble.Show
  ( showGame
  , showPlayer
  , showBoard )
import Scrabble.Lang.Dict
  ( englishDictionaryT )
import Scrabble.Evaluator
  ( Evaluator(..) )
import Scrabble.Lang.Word
  ( stringToWord )
import Scrabble.Lang.Search
  ( findPrefixesT )

-- | Start a new game.
startGame :: Text -- ^ Name of Player 1
          -> Text -- ^ Name of Player 2
          -> IO ()
startGame p1Name p2Name = do
  theGen <- getStdGen
  d      <- englishDictionaryT
  _ <- playGame (newGame p1Name p2Name theGen d)
  return ()

-- | Start a new game against the computer.
startGameAI :: Text -- ^ Name of Player 1
            -> IO ()
startGameAI p1Name = do
  theGen <- getStdGen
  d      <- englishDictionaryT
  _ <- playGame (newGame1P p1Name theGen d)
  return ()

-- | Play the game.
playGame :: Game -> IO Game
playGame g = do
  printBoard False (board g) Nothing
  printPlayer $ player1 g
  printPlayer $ player2 g
  takeTurn g Nothing

-- | Take a turn.
takeTurn :: Game -- ^ The game
         -> Maybe Text -- ^ Previous score as text
         -> IO Game
takeTurn g msc = trace ("Turn: " ++ show (turn g)) $ runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     liftIO $ printBoard False (board g) msc
     if gameOver g
       then liftIO $ doGameOver g
       else if isAI (getPlayer g)
            then liftIO $ takeTurnAI g
            else liftIO $ takeTurnManual g

-- | Allow the computer to take a turn.
takeTurnAI :: Game -> IO Game
takeTurnAI g = case moveAI valGameRulesAndDict g of
  Ev (Right (g',i)) -> takeTurn g' (Just (T.pack $ show i))
  Ev (Left e)       -> do T.putStrLn e
                          pure g

-- | Take a turn manually.
takeTurnManual :: Game -- ^ The game
               -> IO Game
takeTurnManual g = runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     mLn <- getInputLine (T.unpack $ showTurn g)
     case mLn of
       Nothing -> loop
       Just wdStr -> do
         let wds = words wdStr
         if length wds /= 4 && head (head wds) /= ':'
           then loop
           else do
           (wd,is) <- liftIO $ replaceBlanks (head wds)
           if head wd == ':'
             then liftIO $ do
             (g',ms) <- cmd (T.map toUpper (T.pack wd), T.pack <$> mLn, g)
             _ <- traceIO ("LMP:" ++ show (lastMovePass g'))
             takeTurn g' ms
             else do
             let [rowStr,colStr,dirStr] = tail $ words wdStr
                 row = read rowStr :: Int
                 col = read colStr :: Int
                 dir = if map toUpper dirStr == "H" then HZ else VT
                 wp  = mkWP wd (row,col) dir is 
             case move valGameRules g wp is of
               Ev (Left e) -> do liftIO $ T.putStrLn e
                                 liftIO $ takeTurn g $ Just ((T.pack wd)  <> ": NO SCORE")
               Ev (Right (g',sc)) -> liftIO $ takeTurn g' (Just (T.pack $ show sc))

-- | Handle the situation when the game ends.
doGameOver :: Game -> IO Game
doGameOver g = do
  let p1     = player1 g
      p2     = player2 g
      draw   = score p1 == score p2
      winner = if score p1 > score p2
               then p1 else p2
  T.putStrLn "Game over!"
  T.putStrLn $ name p1 <> ": " <> (T.pack $ show (score p1))
  T.putStrLn $ name p2 <> ": " <> (T.pack $ show (score p2))
  if draw
    then T.putStrLn "It's a draw!" >> pure g
    else T.putStrLn ("Congratulations " <> name winner) >> pure g

-- | Datatype for commands entered by the user.
data Cmd = Swap | Pass | Hint | Help | Unknown deriving (Show, Eq)

-- | Read a command.
getCmd :: Text -> Cmd
getCmd ":SWAP" = Swap
getCmd ":PASS" = Pass
getCmd ":HINT" = Hint
getCmd ":HELP" = Help
getCmd _       = Unknown

-- | Deal with commands entered by a player
cmd :: (Text, Maybe Text, Game) -> IO (Game, Maybe Text)
cmd (s, mLn, g) = case getCmd s of
                    Swap    -> doSwap (g, mLn)
                    Pass    -> doPass (g, mLn) 
                    Hint    -> do hints g
                                  return (g, mLn)
                    Help    -> do help
                                  return (g, mLn)
                    Unknown -> return (g, mLn)

-- | Take a move by swapping some tiles.
doSwap :: (Game, Maybe Text) -> IO (Game, Maybe Text)
doSwap (g, mLn) = do
  putStrLn "Enter tiles to swap and type return when done:"
  ln <- getLine
  case swap (fromJust $ stringToWord (map toUpper ln)) g of
    Ev (Right g') -> pure (g',mLn)
    Ev (Left e)   -> do T.putStrLn e
                        pure (g,mLn)

-- | Take a move by passing.
doPass :: (Game, Maybe Text) -> IO (Game, Maybe Text)
doPass (g, mLn) = case pass g of
  Ev (Right g') -> pure (g', Just "Passed move")
  Ev (Left e)   -> do T.putStrLn e
                      pure (g,mLn)

-- | Print the help message.
--   TODO
help :: IO ()
help = T.putStrLn "HELP: TODO"

-- | Print some word suggestions based ont hte current player's rack.
hints :: Game -> IO ()
hints g = do
  let w = rack (getPlayer g) 
  T.putStrLn "HINTS:"
  mapM_ print $ findPrefixesT (dict g) w
               
-- | Interactively query for the value of blanks that have been played.
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

-- | Print the board.
printBoard :: Bool -> Board -> Maybe Text -> IO ()
printBoard printBonuses b msc = do T.putStrLn $ showBoard printBonuses b
                                   forM_ msc T.putStrLn

-- | Print the board and the current player.
printGame :: Bool -> Board -> Player -> IO ()
printGame printBonuses b p = T.putStrLn $ showGame printBonuses b p

-- | Print the current player.
printPlayer :: Player -> IO ()
printPlayer p = T.putStrLn $ showPlayer p

-- | Print the prompt for the current turn.
printTurn :: Game -> IO ()
printTurn g = T.putStrLn $ showTurn g

-- | Print the board and the prompt for the current turn.
printBoardAndTurn :: Game -> Maybe Text -> IO ()
printBoardAndTurn g msc = do printBoard False (board g) msc
                             printTurn g

-- | Textify the current turn.
showTurn :: Game -> Text
showTurn g = let p = getPlayer g in
  showPlayer p <> "Enter WORD ROW COL DIR[H/V]:\n"

-- find the indices of occurences of the first argument in the second argument.
indices :: Eq a => a -> [a] -> [Int]
indices x xs = map fst $ filter ((==x) . snd) (zip [0..14] xs)

-- count the number of elements in a list.
countElem :: Eq a => a -> [a] -> Int
countElem _ []     = 0
countElem x (y:ys) = if x==y then 1 + countElem x ys else countElem x ys

main :: IO ()
main = do
  T.putStrLn "Enter name of Player 1"
  p1Str <- T.getLine
  T.putStrLn "Enter name of Player 2"
  p2Str <- T.getLine
  startGame p1Str p2Str

