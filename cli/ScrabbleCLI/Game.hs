module ScrabbleCLI.Game ( startGame, startGameAI ) 
where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Char (toUpper)
import System.Console.Haskeline
import System.Random (getStdGen)
import Scrabble.Lang.Search (findPrefixes)
import Scrabble.Lang.Word (stringToWord)
import Scrabble.Lang.Dict (englishDictionary)
import Scrabble.Board.Board (mkWP)
import Scrabble.Game.Game
  ( newGame
  , getPlayer
  , move
  , swap
  , pass )
import Scrabble.Game.AI (moveAI, newGame1P)
import Scrabble.Game.Validation (valGameRules)
import Scrabble.Types
  ( Game(..)
  , Player(..)
  , Evaluator(..)
  , Dir(..))
import ScrabbleCLI.Out
  ( printBoard
  , printPlayer
  , showTurn )
import ScrabbleCLI.Blanks (replaceBlanks)

-- =========== Playing a CLI game =============== --

-- | Start a new game.
startGame :: Text -- ^ Name of Player 1
          -> Text -- ^ Name of Player 2
          -> IO ()
startGame p1Name p2Name = do
  theGen <- getStdGen
  d      <- englishDictionary
  _ <- playGame (newGame p1Name p2Name theGen d)
  return ()

-- | Start a new game against the computer.
startGameAI :: Text -- ^ Name of Player 1
            -> IO ()
startGameAI p1Name = do
  theGen <- getStdGen
  d      <- englishDictionary
  _ <- playGame (newGame1P p1Name theGen d)
  return ()

-- | Play the game.
playGame :: Game -> IO Game
playGame g = do
  printPlayer $ player1 g
  printPlayer $ player2 g
  takeTurn g Nothing

-- | Take a turn.
takeTurn :: Game -- ^ The game
         -> Maybe Text -- ^ Previous score as text
         -> IO Game
takeTurn g msc = runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     liftIO $ printBoard True (board g) msc
     if gameOver g
       then liftIO $ doGameOver g
       else if isAI (getPlayer g)
            then liftIO $ takeTurnAI g
            else liftIO $ takeTurnManual g

-- | Allow the computer to take a turn.
takeTurnAI :: Game -> IO Game
takeTurnAI g = case moveAI g of
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
         if null wds || (length wds /= 4 && head (head wds) /= ':')
           then loop
           else do
           (wd,is) <- liftIO $ replaceBlanks (head wds)
           if head wd == ':'
             then liftIO $ do
             (g',ms) <- cmd (T.map toUpper (T.pack wd), T.pack <$> mLn, g)
             takeTurn g' ms
             else do
             let [rowStr,colStr,dirStr] = tail $ words wdStr
                 row = read rowStr :: Int
                 col = read colStr :: Int
                 dir = if map toUpper dirStr == "H" then HZ else VT
                 wp  = mkWP wd (row,col) dir is
             case move valGameRules g wp is of
               Ev (Left e) -> do liftIO $ T.putStrLn e
                                 liftIO $ takeTurn g $ Just (T.pack wd  <> ": NO SCORE")
               Ev (Right (g',(_,sc))) -> liftIO $ takeTurn g' (Just (T.pack $ show sc))

-- | Handle the situation when the game ends.
doGameOver :: Game -> IO Game
doGameOver g = do
  let p1     = player1 g
      p2     = player2 g
      draw   = score p1 == score p2
      winner = if score p1 > score p2
               then p1 else p2
  T.putStrLn "Game over!"
  T.putStrLn $ name p1 <> ": " <> T.pack (show (score p1))
  T.putStrLn $ name p2 <> ": " <> T.pack (show (score p2))
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
  mapM_ print $ findPrefixes (dict g) w
