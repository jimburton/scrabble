{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server
  ( Browsing(EnableBrowsing)
  , nullConf
  , serveDirectory
  , simpleHTTP
  )

-- ================= the client web app just serves static files ================= --

-- | Serves the static files in html/ on port 8000
main :: IO ()
main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "web/client/html"
