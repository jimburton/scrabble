module ScrabbleCLI.Out
  ( printBoard
  , printPlayer
  , showTurn )
  where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Foldable (forM_)
import Lens.Simple ((^.))
import Scrabble.Types
  ( Game
  , Player
  , Board )
import Scrabble.Game.Game (getPlayer)
import Scrabble.Board.Pretty
  ( showPlayer 
  , showBoard )
       
-- ========= Output for CLI Scrabble games ========== --  

-- | Print the board.
printBoard :: Bool -> Board -> Maybe Text -> IO ()
printBoard printBonuses b msc = do T.putStrLn $ showBoard printBonuses b
                                   forM_ msc T.putStrLn

-- | Print the current player.
printPlayer :: Player -> IO ()
printPlayer p = T.putStrLn $ showPlayer p

-- | Textify the current turn.
showTurn :: Game -> Text
showTurn g = let p = g ^. getPlayer g in
  showPlayer p <> "Enter WORD ROW COL DIR[H/V]:\n"

