module Test.Chapter3
  ( prop_straight
  , prop_wordOnBoard
  , prop_fMCentre
  , prop_connects
  , prop_lettersAvailable)
  where

import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick)
import Control.Monad.IO.Class (liftIO)
import System.Random (getStdGen)
import Lens.Simple ((^.))
import Data.Bifunctor (first)
import Scrabble.Types
  ( Evaluator(..)
  , Letter(..)
  , Dir(..)
  , board, player1, player2, rack
  , WordPut
  , Game )
import Scrabble.Board.Board
  ( updateBoard
  , incRow
  , makeWordPut )
import Scrabble.Lang.Word (wordToText)
import Scrabble.Board.Pretty()
import Scrabble.Board.Validation
  ( validateMove )  
import Scrabble.Lang.Dict (englishDictionary)
import Test.Gen (genGame)

-- ========= Tests for Chapter 3 ========== --

-- | Make a @WordPut@ from Player 1's rack.
p1Word :: Game -> WordPut
p1Word g = makeWordPut (wordToText $ filter (/= Blank) (g ^. (player1 . rack))) (7,7) HZ []

-- | Test the @wordOnBoard@ validation.
prop_wordOnBoard :: Property 
prop_wordOnBoard = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let wp = p1Word g
  case validateMove (g ^. board) (g ^. player1) wp True of
    Ev (Right _) -> assert True
    Ev (Left e)  -> do liftIO $ print e
                       assert False
  let e = makeWordPut (wordToText $ g ^. (player1 . rack)) (10,7) HZ []
  case validateMove (g ^. board) (g ^. player1) e False of
    Ev (Right _) -> assert False
    Ev (Left _)  -> assert True

-- | Test the @firstMoveTouchesCentre@ validation.
prop_fMCentre :: Property 
prop_fMCentre = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let fm1 = [((0,12),(A,1)), ((0,13),(B,2)), ((0,14),(C,3))]
  case validateMove (g ^. board) (g ^. player1) fm1 True of
    Ev (Right _) -> assert False
    Ev (Left _)  -> assert True
  let fm2 = p1Word g
  case validateMove (g ^. board) (g ^. player1) fm2 True of
    Ev (Right _) -> assert True
    Ev (Left _)  -> assert False

-- | Test the the @connects@ validation.
--   Puts a @WordPut@ onto the board than creates another which
--   connects with it.
prop_connects :: Property
prop_connects = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let wp = p1Word g
      Ev (Right g') = updateBoard wp g
      start = fst $ head wp
      ts = take 3 $ zip (g' ^. (player2 . rack)) [1..]
      wp2 = head wp : take 3 (zip (iterate incRow (incRow start)) ts)
      wp3 = take 3 $ zip (iterate incRow (incRow (incRow start))) ts
  case validateMove (g' ^. board) (g' ^. player2) wp2 False of
    Ev (Right _) -> assert True
    Ev (Left _)  -> assert False
  case validateMove (g' ^. board) (g' ^. player2) wp3 False of
    Ev (Right _) -> assert False
    Ev (Left _)  -> assert True

-- | Test the @straight@ validation.
prop_straight :: Property 
prop_straight = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let wp1 = p1Word g
      wp2 = init wp1 ++ [first incRow (last wp1)] 
  case validateMove (g ^. board) (g ^. player1) wp1 True of
    Ev (Right _) -> assert True
    Ev (Left _)  -> assert False
  case validateMove (g ^. board) (g ^. player1) wp2 True of
    Ev (Right _) -> assert False
    Ev (Left _)  -> assert True

-- | Test the @lettersAvailable@ validation.
prop_lettersAvailable :: Property 
prop_lettersAvailable = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let r = g ^. (player1 . rack)
      wp1 = p1Word g
      wp2 = makeWordPut (wordToText $ take 7 (filter (not . (`elem` r)) [A .. Z])) (7,7) HZ []
  case validateMove (g ^. board) (g ^. player1) wp1 True of
    Ev (Right _) -> assert True
    Ev (Left _)  -> assert False
  case validateMove (g ^. board) (g ^. player1) wp2 True of
    Ev (Right _) -> assert False
    Ev (Left _)  -> assert True
