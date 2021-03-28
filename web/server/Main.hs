{-# Language OverloadedStrings #-}
module Main where

import Data.Aeson (decode)
import Control.Concurrent (forkIO, threadDelay)
import qualified Network.WebSockets as WS
import Control.Concurrent.BoundedChan
import qualified Data.Text as T
import System.Log.Logger 
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

import ScrabbleWeb.Game (gameStarter, aiGame)
import ScrabbleWeb.Conf (parseConf, defaultConf)
import ScrabbleWeb.Types (Msg(MsgJoin), Client, Conf(..))

-- =========== Entry point for the web server ========= --

-- | Command-line options for the server
newtype Options = Options {
    optConf :: String
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optConf  = "./etc/scrabble.conf"
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "c" ["conf"]
        (ReqArg
            (\arg opt -> return opt { optConf = arg })
            "FILE")
        "Config file"
  , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
  ]
     
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
 
-- | Entry point for the server. It begins by reading the command-line
--   options, setting up the logger and reading the config file. It then
--   creates the Chan which will hold incoming clients, starts the thread
--   that will watch the Chan and start games, and continues to listen for
--   connections.
main :: IO ()
main = do
  args <- getArgs
  -- Parse options, getting a list of option actions
  let (actions, _, _) = getOpt RequireOrder options args
  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optConf = path } = opts
  conf <- parseConf path defaultConf
  let pr = log_priority conf
  updateGlobalLogger "Scrabble" (setLevel pr)
  h <- fileHandler (T.unpack $ log_file conf) pr >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "Scrabble" (addHandler h)
  infoM "Scrabble" ("Starting server with conf "<>show conf)
  state <- newBoundedChan 2
  _ <- forkIO (gameStarter state)
  WS.runServer (T.unpack $ hostname conf) (port conf) $ enqueue state

