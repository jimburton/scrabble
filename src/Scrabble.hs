{-|
Module      : Scrabble
Description : The scrabble library.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

The scrabble library.
-}
module Scrabble (
  module Scrabble.Types
  , module Scrabble.Evaluator
  , module Scrabble.Game.Game
  , module Scrabble.Game.Validation
  , module Scrabble.Board.Board
  , module Scrabble.Board.Bag
  , module Scrabble.Board.Bonus
  , module Scrabble.Board.Pretty
  , module Scrabble.Board.Validation
  , module Scrabble.Lang.Dict
  , module Scrabble.Lang.Search
  , module Scrabble.Lang.Letter
  , module Scrabble.Lang.Word
) where


import Scrabble.Types
import Scrabble.Evaluator
import Scrabble.Game.Game
import Scrabble.Game.Validation
import Scrabble.Board.Board
import Scrabble.Board.Bag
import Scrabble.Board.Bonus
import Scrabble.Board.Pretty
import Scrabble.Board.Validation
import Scrabble.Lang.Dict
import Scrabble.Lang.Search
import Scrabble.Lang.Letter
import Scrabble.Lang.Word
      


