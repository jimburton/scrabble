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
  ( newGame )
import ScrabbleWeb.Types
  ( WebGame(..)
  , Game(..)
  , Client
  , ServerState
  , Msg(..)
  , Turn(..))

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
    writeChan state (msg,conn)
    WS.withPingThread conn 30 (return ()) loop
      where loop = threadDelay (10000*5) >> loop
 
-- | Watch the list of connections and start games.
gameStarter :: BoundedChan Client -> IO ()
gameStarter state = loop
  where loop = do
          c1 <- readChan state
          c2 <- readChan state
          d <- englishDictionaryT
          theGen <- getStdGen
          let ig = G.newGame (fst c1) (fst c2) theGen d
          forkIO $ playGame (newGame c1 c2 ig)
          loop

playGame :: WebGame -> IO ()
playGame wg = do
  _ <- takeTurn wg (Just "New game")
  return ()

announceMove :: WebGame -> IO ()
announceMove wg = do
  let cur = if turn (theGame wg) == P1 then fst (p1 wg) else fst (p2 wg)
  announce wg (cur <> "'s move")

announce :: WebGame -> Text -> IO ()
announce wg txt = do
  let msg = encode $ MsgAnnounce txt
  WS.sendTextData (snd (p1 wg)) msg
  WS.sendTextData (snd (p2 wg)) msg
  
maybeAnnounce :: WebGame -> Maybe Text -> IO ()
maybeAnnounce _ Nothing     = pure ()
maybeAnnounce wg (Just txt) = announce wg txt

-- | Take a turn.
takeTurn :: WebGame -- ^ The game
         -> Maybe Text -- ^ Previous score as text
         -> IO Game
takeTurn wg msc = do
     maybeAnnounce wg msc
     let g = theGame wg
     if gameOver g
       then doGameOver wg
       else if isAI (getPlayer g)
            then takeTurnAI wg
            else takeTurnManual wg

{- 
handleGame g state = do
  let conn = snd (p1 g)
  msg <- WS.receiveData conn
  clients <- readMVar state
  case msg of
    _   | not (prefix `T.isPrefixOf` msg) ->
            WS.sendTextData conn ("Wrong announcement" :: Text)
        | any ($ fst client)
          [T.null, T.any isPunctuation, T.any isSpace] ->
            WS.sendTextData conn ("Name cannot " <>
                                   "contain punctuation or whitespace, and " <>
                                   "cannot be empty" :: Text)
        | clientExists client clients ->
            WS.sendTextData conn ("User already exists" :: Text)
        | otherwise -> flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
              let s' = addGame g s
              WS.sendTextData conn $
                "Welcome! Users: " <>
                T.intercalate ", " (map (\g -> fst (p1 g) `T.append` ", " `T.append` fst (p2 g)) s)
              broadcast (fst client <> " joined") s'
              return s'
            talk client state
                where
                  prefix     = "Hi! I am "
                  client     = (T.drop (T.length prefix) msg, conn)
                  disconnect = do
                    -- Remove client and return new state
                    s <- modifyMVar state $ \s ->
                      let s' = removeGame g s in return (s', s')
                    broadcast (fst client <> " disconnected") s

-}

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)

newGame :: Client -> Client -> Game -> WebGame
newGame c1 c2 g = WebGame c1 c2 g
