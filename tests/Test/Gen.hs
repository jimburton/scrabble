module Test.Gen
  ( genTile
  , genDir
  , genPos
  , genWordPutElement
  , genWordPutStart
  , genWordPut
  , genGame )
  where

import Test.QuickCheck
import Test.QuickCheck.Gen (Gen)
import qualified Data.Text as T
import Data.Text (Text)
import System.Random (getStdGen, StdGen)

import Scrabble.Types
  ( Game
  , Tile
  , Pos
  , Letter(..)
  , Dir(..)
  , Dict )
import Scrabble.Dict
  ( scoreLetter
  , englishDictionary )
import Scrabble.Game (newGame)
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
genWordPut :: Gen (Pos, Tile)
genWordPut = do
  t <- genTile
  p <- genPos (5,5)
  pure (p,t)

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



