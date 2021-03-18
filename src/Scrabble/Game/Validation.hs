module Scrabble.Game.Validation
  ( valGameRulesAndDict
  , valGameRules )
  where

import Debug.Trace
import Scrabble.Types
  ( Validator
  , Game(..)
  , Player(..) )
import Scrabble.Game.Internal
  ( getPlayer )
import Scrabble.Lang.Word
  ( wordToText )
import Scrabble.Lang.Dict
  ( wordsInDictM )
import Scrabble.Board.Validation
  ( validateRack
  , validateMove )

-- ======== Validation of games ======== --

-- | Validate a set of words against the rules of the game and the dictionary. 
valGameRulesAndDict :: Validator
valGameRulesAndDict ws g = do
  let d  = dict g
      ts = map (wordToText . map (fst .snd)) ws
  valGameRules ws g >> wordsInDictM d ts 

-- | Validate a set of words against the rack (are all tiles in the current
--   player's rack or on the board?) and that the move is in the rules of the
--   game (each word except the first one connects to an existing word, whereas the
--   first one to be played must touch the centre square)
valGameRules :: Validator
valGameRules ws g = do
  let b  = board g
      p  = getPlayer g
      w  = head ws
      fm = firstMove g
  validateRack b (rack p) w >> validateMove b p w fm
