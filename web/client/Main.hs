{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Main.hs
-- Description : Entry point for web server.
-- Maintainer  : jimburton1@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
--
module Main where

import Happstack.Server
  ( Browsing(EnableBrowsing)
  , nullConf
  , serveDirectory
  , simpleHTTP
  )

-- * The client web app just serves static files.

-- | Serves the static files in html/ on port 8000
main :: IO ()
main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "web/client/html"
