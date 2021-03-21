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
 , module Scrabble.Game.Game
 , module Scrabble.Game.AI
 , module Scrabble.Board.Board
 , module Scrabble.Board.Bag
 , module Scrabble.Board.Pretty
 , module Scrabble.Lang.Dict
 , module Scrabble.Lang.Search
 , module Scrabble.Lang.Letter
 , module Scrabble.Lang.Word
) where


import Scrabble.Types
import Scrabble.Game.Game
import Scrabble.Game.AI
import Scrabble.Board.Board
import Scrabble.Board.Bag
import Scrabble.Board.Pretty
import Scrabble.Lang.Dict
import Scrabble.Lang.Search
import Scrabble.Lang.Letter
import Scrabble.Lang.Word
