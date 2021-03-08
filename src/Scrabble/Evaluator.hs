module Scrabble.Evaluator ( Evaluator(..)
                          , evalBool )
  where

import qualified Data.Text as T
import Scrabble.Types ( Evaluator(..) )
-- import Control.Monad.IO.Class ( liftIO )

instance Functor Evaluator where
  -- fmap :: (a -> b) -> f a -> f b 
  fmap _ (Ev (Left e))  = Ev (Left e)
  fmap f (Ev (Right g)) = Ev (Right (f g))

instance Applicative Evaluator where
  -- pure :: a -> f a
  pure k = Ev (Right k)
  -- (<*>) :: f (a -> b) -> f a -> f b
  Ev (Left  e)  <*>  _  =  Ev (Left e)
  Ev (Right f)  <*>  r  =  fmap f r

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg)
          Right v  -> k v
    return   = pure
    fail msg = Ev (Left (T.pack msg))

-- | Test a bool in the monad
evalBool :: Bool -> String -> Evaluator Bool
evalBool b e = if b then pure True else fail e

--try :: IO (String -> g) -> IO (g -> g) -> Evaluator g -> IO g
--try k r e = case e of
--              (Ev (Left e))  -> e <$> k 
--              (Ev (Right g)) -> r g

