{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Random ( getStdGen )
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Scrabble.Lang.Dict
  ( englishDictionaryT )
import qualified Scrabble.Game.Game as G
  ( newGame )
import ScrabbleWeb.Types
  ( WebGame(..)
  , Game
  , Client
  , ServerState )

newServerState :: ServerState
newServerState = []

numGames :: ServerState -> Int
numGames = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any (\g -> fst client == fst (p1 g) || fst client == fst (p2 g))

-- | Add a client (this does not check if the client already exists, you should do
--   this yourself using `clientExists`):

addGame :: WebGame -> ServerState -> ServerState
addGame g games = g : games

removeGame :: WebGame -> ServerState -> ServerState
removeGame g = filter (/= g)

broadcast :: Text -> ServerState -> IO ()
broadcast message gs = do
    T.putStrLn message
    forM_ gs $ \g -> do WS.sendTextData (snd (p1 g)) message
                        WS.sendTextData (snd (p2 g)) message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

-- | Our main application has the type:
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn1 <- WS.acceptRequest pending
    name1 <- WS.receiveData conn1
    conn2 <- WS.acceptRequest pending
    name2 <- WS.receiveData conn2
    theGen <- getStdGen
    d <- englishDictionaryT
    let c1 = (name1,conn1)
        c2 = (name2,conn2)
        g = newGame c1 c2 (G.newGame name1 name2 theGen d)
    WS.withPingThread conn1 30 (return ()) (handleGame g state)

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

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)

newGame :: Client -> Client -> Game -> WebGame
newGame c1 c2 g = WebGame c1 c2 g
