{-# LANGUAGE OverloadedStrings #-}
module ScrabbleWeb.Game
  ( gameStarter
  , playGame
  , newGame )
  where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromJust)
import Data.List (find)
import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan
import Data.Aeson
import System.Random (getStdGen)
import ScrabbleWeb.Types
  ( WebGame(..)
  , Client
  , Msg(..)
  , Move(..))
import Scrabble.Types
  ( Evaluator(..)
  , Game(..)
  , Turn(..)
  , Player(..))
import Scrabble.Lang.Dict (englishDictionaryT)
import Scrabble.Lang.Word (wordPutToText)
import qualified Scrabble.Game.Game as G
  ( move
  , getPlayer
  , newGame )
import Scrabble.Game.AI (moveAI)
import Scrabble.Game.Validation
  ( valGameRules
  , valGameRulesAndDict )
import ScrabbleWeb.Announce
  ( announce
  , announceScores
  , announceTurn
  , maybeAnnounce
  , sendRack
  , sendJoinAcks
  , msgOpponent )

-- ========== Playing a game on the web ================ --

-- | Watch the Channel of connections and start a game in a
--   new thread when there are two clients waiting.
gameStarter :: BoundedChan Client -> IO ()
gameStarter state = loop
  where loop = do
          (n1,c1) <- readChan state
          (n2,c2) <- readChan state
          let (n1',n2') = distinctNames (n1,n2)
          T.putStrLn (n1' <> " vs " <> n2')
          d <- englishDictionaryT
          theGen <- getStdGen
          let ig = G.newGame n1' n2' theGen d
          _ <- forkIO $ playGame (newGame (n1',c1) (n2',c2) ig)
          loop

distinctNames :: (Text,Text) -> (Text,Text)
distinctNames (n1,n2) =
  (n1, fromJust $ find (/=n1) (n2 : zipWith (\n i -> n <> (T.pack $ show i)) (repeat n2) [1..]))
  
newGame :: Client -> Client -> Game -> WebGame
newGame = WebGame

playGame :: WebGame -> IO ()
playGame wg = do
  sendJoinAcks wg 
  _ <- takeTurn wg Nothing
  return ()

-- | Take a turn.
takeTurn :: WebGame    -- ^ The game
         -> Maybe Text -- ^ A message such as the previous score or a description of an error
         -> IO WebGame
takeTurn wg msc = do
     maybeAnnounce wg msc
     announceTurn wg
     let g = theGame wg
     if gameOver g
       then doGameOver wg
       else if isAI (G.getPlayer g)
            then takeTurnAI wg
            else takeTurnManual wg

-- | Allow the computer to take a turn.
takeTurnAI :: WebGame -> IO WebGame
takeTurnAI wg = do
  let g = theGame wg
  case moveAI g of
    Ev (Right (g',i)) -> takeTurn wg { theGame = g' } (Just (T.pack $ show i))
    Ev (Left e)       -> do announce wg e
                            pure wg

-- | Take a turn manually.
takeTurnManual :: WebGame -> IO WebGame
takeTurnManual wg = do
  o <- decode <$> WS.receiveData (snd $ getClient wg)
  T.putStrLn ("Received: \n" <> T.pack (show o))
  case o of
    Nothing  -> takeTurnManual wg
    Just msg -> case msg of
      MsgHint _  -> doHints wg >> takeTurn wg Nothing 
      MsgPass    -> doPass wg >> takeTurn wg (Just "pass") 
      MsgSwap _  -> doSwap wg >> takeTurn wg (Just "swap") 
      MsgMove (Move wp) -> do
        let g = theGame wg
            w = wordPutToText wp
        case G.move valGameRules g wp [] of
          Ev (Left e) -> do announce wg e
                            takeTurn wg $ Just (w  <> ": NO SCORE")
          Ev (Right (g',sc)) -> do msgOpponent wg msg
                                   takeTurn (wg { theGame = g' }) (Just (T.pack $ show sc))
      _             -> takeTurn wg (Just ("Not expecting that: " <> T.pack (show msg)))

-- | Handle the situation when the game ends.
doGameOver :: WebGame -> IO WebGame
doGameOver wg = do
  let g      = theGame wg
      pl1    = player1 g
      pl2    = player2 g
      draw   = score pl1 == score pl2
      winner = if score pl1 > score pl2
               then pl1 else pl2
  announce wg "Game over!"
  announceScores wg
  if draw
    then announce wg "It's a draw!" >> pure wg
    else announce wg ("Congratulations " <> name winner) >> pure wg

-- | Send hints to a player.
doHints :: WebGame -> IO WebGame
doHints = return

-- | Let the player take a move by passing.
doPass :: WebGame -> IO WebGame
doPass = return

-- | Let the player take a move by swapping some tiles.
doSwap :: WebGame -> IO WebGame
doSwap = return

-- | Get client whose turn it is. 
getClient :: WebGame -> Client
getClient wg = if turn (theGame wg) == P1 then p1 wg else p2 wg
