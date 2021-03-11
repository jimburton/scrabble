module ScrabbleWeb.Announce
  ( announce
  , announceScores
  , msgOpponent
  , announceTurn
  , maybeAnnounce
  , sendRack)
  where

import qualified Network.WebSockets as WS
import Data.Aeson
import Data.Text (Text)
import Scrabble.Game.Game (getPlayer)
import Scrabble.Types
  ( Player(..) )
import ScrabbleWeb.Types
  ( WebGame(..)
  , Game(..)
  , Turn(..)
  , Msg(..)
  , Score(..) )
  
-- ====== Sending messages to clients =========== --
announceTurn :: WebGame -> IO ()
announceTurn wg = do
  let cur = if turn (theGame wg) == P1 then fst (p1 wg) else fst (p2 wg)
  announce wg (cur <> "'s move")

announceScores :: WebGame -> IO ()
announceScores wg = do
  let pl1 = player1 (theGame wg)
      pl2 = player2 (theGame wg)
      s1  = Score P1 $ score pl1
      s2  = Score P2 $ score pl2
  msg wg (MsgScore (s1,s2))

msg :: WebGame -> Msg -> IO ()
msg wg msg = do
  let o = encode msg
  WS.sendTextData (snd (p1 wg)) o
  WS.sendTextData (snd (p2 wg)) o

announce :: WebGame -> Text -> IO ()
announce wg txt = msg wg (MsgAnnounce txt)
  
maybeAnnounce :: WebGame -> Maybe Text -> IO ()
maybeAnnounce _ Nothing     = pure ()
maybeAnnounce wg (Just txt) = announce wg txt

msgOpponent :: WebGame -> Msg -> IO ()
msgOpponent wg msg = do
  let client = if turn (theGame wg) == P1 then p2 wg else p1 wg
  WS.sendTextData (snd client) (encode msg)

msgOne :: Msg -> WebGame -> Turn -> IO ()
msgOne msg wg turn = do
  let conn = if turn == P1 then snd (p1 wg) else snd (p2 wg)
  WS.sendTextData conn (encode msg)

sendRack :: WebGame -> Turn -> IO ()
sendRack wg turn = do
  let pf = if turn == P1 then player1 else player2
      r  = rack (pf (theGame wg))
  msgOne (MsgRack r) wg turn