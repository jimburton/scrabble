{-|
Module      : Scrabble.Evaluator
Description : Typeclass instances for the @Evaluator@ type.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Typeclass instances (@Functor@, @Applicative@ and @Monad@)for the @Evaluator@ type.
@Evaluator@ evaluates scrabble computations and wraps an @Either@ type for error
reporting.
-}
module Scrabble.Evaluator
  ( Evaluator(..)
  , evalBool )
  where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Scrabble.Types (Evaluator(..))

-- * The Evaluator instances

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
    -- (>>=) :: m a -> (a -> m b) -> m b
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg)
          Right v  -> k v
    return   = pure
    fail msg = Ev (Left (T.pack msg))

-- | Test a bool in the monad
evalBool :: Bool         -- ^ The condition to be tested.
         -> Text         -- ^ The error message.
         -> Evaluator () -- ^ Returns () unless it fails.
evalBool b e = unless b $ fail (T.unpack e)
