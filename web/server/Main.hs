{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Random ( getStdGen )
import Data.Char
  ( isPunctuation
  , isSpace )
import Data.Aeson
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad
  ( forM_
  , forever)
import Control.Concurrent
  ( MVar
  , newMVar
  , modifyMVar_
  , modifyMVar
  , readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Control.Concurrent.BoundedChan 
import Control.Concurrent
  ( forkIO
  , threadDelay )
import Scrabble.Lang.Dict
  ( englishDictionaryT )
import qualified Scrabble.Game.Game as G
  ( newGame
  , move
  , getPlayer )
import Scrabble.Game.AI (moveAI)
import Scrabble.Lang.Word (wordToString)
import Scrabble.Game.Validation
  ( valGameRules
  , valGameRulesAndDict )
import ScrabbleWeb.Announce
  ( announce
  , msgOpponent
  , announceScores
  , maybeAnnounce
  , announceTurn
  , sendRack )
import ScrabbleWeb.Types
  ( WebGame(..)
  , Game(..)
  , Client
  , ServerState
  , Msg(..)
  , Turn(..)
  , Move(..) )
import Scrabble.Types
  ( Evaluator(..)
  , Player(..)
  , WordPut )

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists c = any ((== fst c) . fst)

-- | Add a client (this does not check if the client already exists, you should do
--   this yourself using `clientExists`):

addClient :: Client -> ServerState -> ServerState
addClient =  (:) 

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((/= fst c) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast msg cs = do
    T.putStrLn msg
    forM_ cs $ \c -> do WS.sendTextData (snd c) msg

main :: IO ()
main = do
    state <- newBoundedChan 2
    forkIO (gameStarter state)
    WS.runServer "127.0.0.1" 9160 $ enqueue state

-- | Puts connections into the queue.
enqueue :: BoundedChan Client -> WS.ServerApp
enqueue state pending = do
    conn <- WS.acceptRequest pending
    msg  <- WS.receiveData conn
    case decode msg of
      Nothing -> WS.sendTextData conn ("Bad input: " <> msg)
      Just (MsgJoin name) -> do
        T.putStrLn ("Accepting: " <> name)
        writeChan state (name,conn)
        WS.withPingThread conn 30 (return ()) loop
          where loop = threadDelay (10000*5) >> loop
 
-- | Watch the list of connections and start games.
gameStarter :: BoundedChan Client -> IO ()
gameStarter state = loop
  where loop = do
          c1 <- readChan state
          c2 <- readChan state
          T.putStrLn (fst c1 <> " vs " <> fst c2)
          d <- englishDictionaryT
          theGen <- getStdGen
          let ig = G.newGame (fst c1) (fst c2) theGen d
          forkIO $ playGame (newGame c1 c2 ig)
          loop

playGame :: WebGame -> IO ()
playGame wg = do
  sendRack wg P1
  sendRack wg P2
  _ <- takeTurn wg (Just "New game")
  return ()

-- | Take a turn.
takeTurn :: WebGame -- ^ The game
         -> Maybe Text -- ^ Previous score as text
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
  case moveAI valGameRulesAndDict g of
    Ev (Right (g',i)) -> takeTurn wg { theGame = g' } (Just (T.pack $ show i))
    Ev (Left e)       -> do announce wg e
                            pure wg

-- | Handle the situation when the game ends.
doGameOver :: WebGame -> IO WebGame
doGameOver wg = do
  let g      = theGame wg
      p1     = player1 g
      p2     = player2 g
      draw   = score p1 == score p2
      winner = if score p1 > score p2
               then p1 else p2
  announce wg "Game over!"
  announceScores wg
  if draw
    then announce wg "It's a draw!" >> pure wg
    else announce wg ("Congratulations " <> name winner) >> pure wg

-- | Take a turn manually.
takeTurnManual :: WebGame -> IO WebGame
takeTurnManual wg = do
  o <- decode <$> WS.receiveData (snd $ getClient wg)
  putStrLn ("Received: " ++ show o)
  case o of
    Nothing  -> takeTurnManual wg
    Just msg -> case msg of
      MsgMove (Move wp r p) -> do
        let g = theGame wg
            w = wordPutToText wp
        case G.move valGameRules g wp [] of
          Ev (Left e) -> do announce wg e
                            takeTurn wg $ Just (w  <> ": NO SCORE")
          Ev (Right (g',sc)) -> do msgOpponent wg msg
                                   takeTurn (wg { theGame = g' }) (Just (T.pack $ show sc))
  
getClient :: WebGame -> Client
getClient wg = if turn (theGame wg) == P1 then p1 wg else p2 wg

newGame :: Client -> Client -> Game -> WebGame
newGame c1 c2 g = WebGame c1 c2 g

wordPutToText :: WordPut -> Text
wordPutToText = T.pack . wordToString . map (fst . snd)
