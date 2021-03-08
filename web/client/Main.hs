{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import Data.Aeson
import ScrabbleWeb.Types
  ( Move
  , Msg(..) )


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        case decode msg of
          Nothing          -> LT.putStrLn ("Bad input: " <> (LTE.decodeUtf8 msg))
          Just m -> case m of
            MsgAnnounce ann -> LT.putStrLn ("Announcement: " <> T.pack (show ann)) 
            MsgRack r       -> LT.putStrLn ("Rack: " <> T.pack (show r))
            _               -> LT.putStrLn ("Unknown message: " <> (LTE.decodeUtf8 msg))

    -- Read from stdin and write to WS
    let loop = do
            line <- getLine
            unless (null line) $ do
              let msg = read line :: Msg
              WS.sendTextData conn (encode msg)
              loop
    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app
