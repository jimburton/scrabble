{-|
Module      : Scrabble.Game.Validation
Description : Validation of Scrabble games.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Validation of Scrabble games.
-}
module Scrabble.Game.Validation
  ( valGameRulesAndDict
  , valGameRules )
  where

import Lens.Simple ((^.))
import Scrabble.Types
  ( Validator
  , dict
  , board
  , firstMove
  , rack)
import Scrabble.Game.Internal (getPlayer)
import Scrabble.Lang.Word (wordToText)
import Scrabble.Lang.Search (wordsInDictM)
import Scrabble.Board.Validation
  ( validateRack
  , validateMove )

-- * Validation of games

-- | Validate a set of words against the rules of the game and the dictionary. 
valGameRulesAndDict :: Validator
valGameRulesAndDict ws g = do
  let ts = map (wordToText . map (fst .snd)) ws
  valGameRules ws g >> wordsInDictM (g ^. dict) ts 

-- | Validate a set of words against the rack (are all tiles in the current
--   player's rack or on the board?) and that the move is in the rules of the
--   game (each word except the first one connects to an existing word, whereas the
--   first one to be played must touch the centre square)
valGameRules :: Validator
valGameRules ws g = do
  let b  = g ^. board
      p  = g ^. getPlayer g
      w  = head ws
      fm = g ^. firstMove 
  validateRack b (p ^. rack) w >> validateMove b p w fm
