module ScrabbleWeb.Announce
  ( announce
  , announceScores
  , msgOpponent
  , msgCurrent
  , announceTurn
  , maybeAnnounce
  , sendRack
  , sendHints )
  where

import qualified Network.WebSockets as WS
import Data.Aeson
import Data.Text (Text)
import Scrabble.Types
  ( Player(..) )
import ScrabbleWeb.Types
  ( WebGame(..)
  , Game(..)
  , Turn(..)
  , Msg(..)
  , Score(..) )
  
-- ====== Sending messages to clients =========== --

-- | Send a message to both players.
msg :: WebGame -> Msg -> IO ()
msg wg m = do
  let o = encode m
  WS.sendTextData (snd (p1 wg)) o
  WS.sendTextData (snd (p2 wg)) o

-- | Send a message to one player, identified by the Turn parameter.
msgOne :: WebGame -> Turn -> Msg -> IO ()
msgOne wg t m = do
  let conn = if t == P1 then snd (p1 wg) else snd (p2 wg)
  WS.sendTextData conn (encode m)

-- | Send a message to the player whose turn it currently is.
msgCurrent :: WebGame -> Msg -> IO ()
msgCurrent wg = msgOne wg (turn (theGame wg)) 

-- | Send a messsage to the player whose turn it currently is not.
msgOpponent :: WebGame -> Msg -> IO ()
msgOpponent wg = msgOne wg (other (turn (theGame wg)))

-- | Get the other player to the one identified by the Turn parameter.
other :: Turn -> Turn
other t = if t == P1 then P2 else P1

-- | Send an announcement to both players.
announce :: WebGame -> Text -> IO ()
announce wg txt = msg wg (MsgAnnounce txt)

-- | Maybe send an announcement to both players.
maybeAnnounce :: WebGame -> Maybe Text -> IO ()
maybeAnnounce _ Nothing     = pure ()
maybeAnnounce wg (Just txt) = announce wg txt

-- | Tell both players whose turn it it is.
announceTurn :: WebGame -> IO ()
announceTurn wg = do
  let cur = if turn (theGame wg) == P1 then fst (p1 wg) else fst (p2 wg)
  announce wg (cur <> "'s move")

-- | Send the scores to both players.
announceScores :: WebGame -> IO ()
announceScores wg = do
  let pl1 = player1 (theGame wg)
      pl2 = player2 (theGame wg)
      s1  = Score P1 $ score pl1
      s2  = Score P2 $ score pl2
  msg wg (MsgScore (s1,s2))

-- | Send the rack to the player identified by the Turn parameter.
sendRack :: WebGame -> Turn -> IO ()
sendRack wg t = do
  let pf = if t == P1 then player1 else player2
      r  = rack (pf (theGame wg))
  msgOne wg t (MsgRack r)

-- | Send hints to the player identified by the Turn parameter.
sendHints :: WebGame -> Turn -> Maybe [Word]  -> IO ()
sendHints wg t ws = msgOne wg t (MsgHint ws)
