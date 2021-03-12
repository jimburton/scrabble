{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import qualified Data.Text.IO as T
import ScrabbleCLI.Game (startGame, startGameAI) 

main :: IO ()
main = do
  T.putStrLn "Enter 1P or 2P"
  str <- T.getLine
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

