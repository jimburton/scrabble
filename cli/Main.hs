{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import Data.Text (toUpper)
import qualified Data.Text.IO as T
import ScrabbleCLI.Game (startGame, startGameAI)

-- ========= Entry point for a CLI game of Scrabble =========== --

main :: IO ()
main = do
  T.putStrLn "Enter 1P or 2P"
  str <- fmap toUpper T.getLine
  case str of
    "1P" -> doAIGame
    "2P" -> doManualGame
    _    -> main

doManualGame :: IO ()
doManualGame = do
  T.putStrLn "Enter name of Player 1"
  p1Str <- T.getLine
  T.putStrLn "Enter name of Player 2"
  p2Str <- T.getLine
  startGame p1Str p2Str

doAIGame :: IO ()
doAIGame = do
  T.putStrLn "Enter name of player"
  pStr <- T.getLine
  startGameAI pStr

