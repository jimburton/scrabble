{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server
  ( Browsing(EnableBrowsing)
  , nullConf
  , serveDirectory
  , simpleHTTP
  )

-- ================= the client web app just serves static files ================= --

main :: IO ()
main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "web/client/html"

{-
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
-}
