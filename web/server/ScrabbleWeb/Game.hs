module ScrabbleWeb.Game
  ( playGame
  , newGame )
  where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Data.Aeson
import ScrabbleWeb.Types
  ( WebGame(..)
  , Client
  , Msg(..)
  , Move(..))
import Scrabble.Types
  ( Evaluator(..)
  , Game(..)
  , WordPut
  , Turn(..)
  , Player(..))
import Scrabble.Lang.Word
  ( wordToString )
import Scrabble.Game.Validation
  ( valGameRules
  , valGameRulesAndDict )
import qualified Scrabble.Game.Game as G
  ( move
  , getPlayer )
import Scrabble.Game.AI
  ( moveAI )
import ScrabbleWeb.Announce
  ( announce
  , announceScores
  , announceTurn
  , maybeAnnounce
  , sendRack
  , msgOpponent )

playGame :: WebGame -> IO ()
playGame wg = do
  sendRack wg P1
  sendRack wg P2
  _ <- takeTurn wg (Just "New game")
  return ()

-- | Take a turn.
takeTurn :: WebGame -- ^ The game
         -> Maybe Text -- ^ Previous score as text
         -> IO WebGame
takeTurn wg msc = do
     maybeAnnounce wg msc
     announceTurn wg
     let g = theGame wg
     if gameOver g
       then doGameOver wg
       else if isAI (G.getPlayer g)
            then takeTurnAI wg
            else takeTurnManual wg

-- | Allow the computer to take a turn.
takeTurnAI :: WebGame -> IO WebGame
takeTurnAI wg = do
  let g = theGame wg
  case moveAI valGameRulesAndDict g of
    Ev (Right (g',i)) -> takeTurn wg { theGame = g' } (Just (T.pack $ show i))
    Ev (Left e)       -> do announce wg e
                            pure wg

-- | Handle the situation when the game ends.
doGameOver :: WebGame -> IO WebGame
doGameOver wg = do
  let g      = theGame wg
      p1     = player1 g
      p2     = player2 g
      draw   = score p1 == score p2
      winner = if score p1 > score p2
               then p1 else p2
  announce wg "Game over!"
  announceScores wg
  if draw
    then announce wg "It's a draw!" >> pure wg
    else announce wg ("Congratulations " <> name winner) >> pure wg

-- | Take a turn manually.
takeTurnManual :: WebGame -> IO WebGame
takeTurnManual wg = do
  o <- decode <$> WS.receiveData (snd $ getClient wg)
  putStrLn ("Received: " ++ show o)
  case o of
    Nothing  -> takeTurnManual wg
    Just msg -> case msg of
      MsgMove (Move wp) -> do
        let g = theGame wg
            w = wordPutToText wp
        case G.move valGameRules g wp [] of
          Ev (Left e) -> do announce wg e
                            takeTurn wg $ Just (w  <> ": NO SCORE")
          Ev (Right (g',sc)) -> do msgOpponent wg msg
                                   takeTurn (wg { theGame = g' }) (Just (T.pack $ show sc))
  
getClient :: WebGame -> Client
getClient wg = if turn (theGame wg) == P1 then p1 wg else p2 wg

newGame :: Client -> Client -> Game -> WebGame
newGame c1 c2 g = WebGame c1 c2 g

wordPutToText :: WordPut -> Text
wordPutToText = T.pack . wordToString . map (fst . snd)
