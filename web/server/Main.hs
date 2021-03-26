{-# LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace
import Data.Aeson (decode)
import Control.Concurrent (forkIO, threadDelay)
import qualified Network.WebSockets as WS
import Control.Concurrent.BoundedChan
import qualified Data.Text as T
import System.Log.Logger 
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import ScrabbleWeb.Game (gameStarter, aiGame) 
import ScrabbleWeb.Types (Msg(MsgJoin), Client)

-- =========== Entry point for the web server ========= --

-- | Makes incoming connections into Clients and adds them to the queue.
enqueue :: BoundedChan Client -> WS.ServerApp
enqueue state pending = do
    conn <- WS.acceptRequest pending
    msg  <- WS.receiveData conn
    case decode msg of
      Nothing -> WS.sendTextData conn ("Bad input: " <> msg)
      Just (MsgJoin (name,ai)) -> do
        infoM "Scrabble" ("[Client] " <> T.unpack name <> " [AI] " <> show ai)
        -- If the client wants an AI game, start right away. Otherwise, put them in the queue.
        if ai
          then WS.withPingThread conn 30 (return ()) (aiGame (name,conn))
          else do
          writeChan state (name,conn) 
          -- keep the connection alive by having a thread that listens to it
          WS.withPingThread conn 30 (return ()) loop
            where loop = threadDelay (10000*5) >> loop
      Just _ -> WS.sendTextData conn ("Not expecting: " <> msg)
 
-- | Entry point for the server. Sets up the logger. Creates the Chan which will hold
--   incoming clients, starts the thread that will watch the Chan and
--   start games, then listens for connections.
main :: IO ()
main = do
  -- Copy everything to syslog from here on out.
  --s <- openlog "SyslogStuff" [PID] USER DEBUG
  --updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "Scrabble" (setLevel DEBUG)
  h <- fileHandler "scrabble.log" DEBUG >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "Scrabble" (addHandler h)
  infoM "Scrabble" "Starting server..."
  state <- newBoundedChan 2
  _ <- forkIO (gameStarter state)
  WS.runServer "127.0.0.1" 9160 $ enqueue state


