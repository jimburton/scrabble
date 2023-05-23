-- |
-- Module      : ScrabbleCLI.Blanks
-- Description : Functions for dealing with blanks in the CLI interface.
-- Maintainer  : jimburton1@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- 
module ScrabbleCLI.Blanks ( replaceBlanks )
  where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, runInputT )
import Control.Monad.IO.Class (liftIO)

-- * Handling blanks in the input

-- | Interactively query for the value of blanks that have been played.
replaceBlanks :: String             -- ^ The string with blanks in it
              -> IO (String, [Int]) -- ^ The unblanked string and the positions that were blanks 
replaceBlanks wd = if countElem '_' wd == 0
                   then return (wd,[])
                   else do ub <- unBlank wd
                           --print (ub, indices '_' wd)
                           return (ub, indices '_' wd)
  where unBlank :: String -> IO String
        unBlank []       = return []
        unBlank ('_':us) = runInputT defaultSettings $ do
          mc <- getInputLine "Enter a letter for the blank:"
          case mc of
            (Just c) -> do rst <- liftIO $ unBlank us
                           return $ head c : rst
            Nothing  -> liftIO $ unBlank ('_':us)   
        unBlank (u:us)   = do rst <- unBlank us
                              return $ u : rst

-- find the indices of occurences of the first argument in the second argument.
indices :: Eq a => a -> [a] -> [Int]
indices x xs = map fst $ filter ((==x) . snd) (zip [0..14] xs)

-- count the number of elements in a list.
countElem :: Eq a => a -> [a] -> Int
countElem _ []     = 0
countElem x (y:ys) = if x==y then 1 + countElem x ys else countElem x ys
