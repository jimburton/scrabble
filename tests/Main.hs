module Main
    where

import Data.Array
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Scrabble.Board (newBoard, updateBoard)
import Scrabble.Types
{-
samplesPath :: FilePath
samplesPath = "etc/samples"

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf1 genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

prop_codecRAWPGM :: SafeString -> Property
prop_codecRAWPGM (SafeString msg) = do let inPath  = samplesPath </> "pgm/RAW/surf.pgm"
                                       prop_codecAny msg inPath "tmp.png" 

prop_codecBMP :: SafeString -> Property
prop_codecBMP (SafeString msg) = do let inPath  = samplesPath </> "bmp/24bit/duck.bmp"
                                    prop_codecAny msg inPath "tmp.bmp"

prop_codecAny :: String -> String -> String -> Property
prop_codecAny msg inPath tmp = monadicIO test
    where test = do run $ bury' inPath tmp (L8.toStrict $ L8.pack (msg++"\n"))
                    readMsg <- run $ dig tmp
                    run $ removeFile tmp
                    assert $ msg == fromJust readMsg
-}

genLetter :: Gen Letter
genLetter = elements [A .. Z]

prop_newBoardSize :: Bool
prop_newBoardSize = bounds newBoard == ((0,0),(14,14))

{-
prop_placeWord :: Property
prop_placeWord = let c   = ((0,0),(C,3))
                     a   = ((0,1),(A,1))
                     t   = ((0,2),(T,1))
                     cat = [c,a,t]
                     b   = updateBoard cat newBoard in
                   assert $ b ! (0,0) == c && b ! (0,1) == a && b ! (0,2) == t 
-}

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "newBoard is the right size" prop_newBoardSize ]

{-
tests = [ testProperty "prop_codecRAWPGM" prop_codecRAWPGM
        , testProperty "prop_codecBMP" prop_codecBMP ]
-}
