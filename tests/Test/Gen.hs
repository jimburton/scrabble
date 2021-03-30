{-# LANGUAGE ScopedTypeVariables  #-}
module Test.Gen
  ( genTile
  , genDir
  , genPos
  , genWordPutElement
  , genWordPutStart
  , genWordPut
  , genGame
  , genGameAI
  , p1Word)
  where

import Test.QuickCheck
import Test.QuickCheck.Gen (Gen)
import qualified Data.Text as T
import Data.Text (Text)
import System.Random (StdGen)
import Data.Bifunctor (first)
import Lens.Simple ((^.))
import Data.Functor ((<&>))
import Scrabble.Types
  ( Game
  , player1, rack
  , Tile
  , Pos
  , Letter(..)
  , Dir(..)
  , Dict
  , WordPut )
import Scrabble.Lang.Word (wordToText)
import Scrabble.Lang.Letter (scoreLetter)
import Scrabble.Game.Game (newGame)
import Scrabble.Game.AI (newGame1P)
import Scrabble.Board.Board
  ( incCol
  , incRow
  , makeWordPut )
import ScrabbleCLI.Game (startGameAI)

-- ======== Generators and instances for tests ============ --

instance Arbitrary Letter where
  arbitrary = chooseEnum (A,Z) 

-- | Generate an arbitrary Tile.
genTile :: Gen Tile
genTile = do
  l <- elements [A .. Z]
  pure (l, scoreLetter l)

-- | Generate an arbitrary direction.
genDir :: Gen Dir
genDir = elements [ HZ, VT]

-- | Generate a pos in the range (0,0) to (ur,uc).
genPos :: (Int,Int) -> Gen Pos
genPos (ur,uc) = do
  r <- choose (0, ur) :: Gen Int
  c <- choose (0, uc) :: Gen Int
  pure (r,c)

-- | Generate an arbitrary element of a WordPut.
genWordPutElement :: Gen (Pos, Tile)
genWordPutElement = do
  t <- genTile
  p <- genPos (14,14)
  pure (p,t)                         

-- | Generate the first element in a WordPut somewhere
-- in the upper left part of the board.
genWordPutStart :: Gen (Pos, Tile)
genWordPutStart = do
  t <- genTile
  p <- genPos (5,5)
  pure (p,t)                         

-- | Generate an arbitrary WordPut.
genWordPut :: Gen WordPut
genWordPut = do
  dir <- genDir
  let inc = if dir == HZ then incCol else incRow
  wps <- genWordPutStart
  (size :: Int)  <- choose (3,9)
  let wp = take size (iterate (first inc) wps)
  pure wp

-- | Generate an arbitrary printable Text
arbitraryPrintableText :: Gen Text
arbitraryPrintableText = do
  s <- getPrintableString <$> arbitrary
  pure $ T.pack s

-- | Generate an arbitrary game.
genGame :: StdGen -> Dict -> Gen Game
genGame g d = do
  n1 <- arbitraryPrintableText
  n2 <- arbitraryPrintableText
  pure (newGame n1 n2 g d)

-- | Generate an arbitrary game.
genGameAI :: StdGen -> Dict -> Gen Game
genGameAI g d = do
  n <- arbitraryPrintableText
  pure (newGame1P n g d)


-- | Make a @WordPut@ from Player 1's rack.
p1Word :: Game -> WordPut
p1Word g = makeWordPut (wordToText $ filter (/= Blank) (g ^. (player1 . rack))) (7,7) HZ []
