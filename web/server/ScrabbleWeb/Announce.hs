module ScrabbleWeb.Announce
  ( announce
  , msgScores
  , msgOpponent
  , msgCurrent
  , msgMoveAck
  , msgTurn
  , maybeAnnounce
  , sendRack
  , sendRackOpponent
  , sendHints
  , sendJoinAcks )
  where

import Prelude hiding (Word)
import qualified Network.WebSockets as WS
import Data.Aeson
import Data.Text (Text)
import Scrabble.Types
  ( Player(..)
  , Word
  , WordPut )
import ScrabbleWeb.Types
  ( WebGame(..)
  , Game(..)
  , Turn(..)
  , Msg(..)
  , Score(..)
  , JoinAck(..)
  , MoveAck(..))
  
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

-- | Tell both players whose turn it is.
msgTurn :: WebGame -> IO ()
msgTurn wg = msg wg (MsgTurn $ turn (theGame wg))

-- | Acknowledge to a legal move, sending the move to both players.
msgMoveAck :: WebGame -> WordPut -> Int -> IO ()
msgMoveAck wg w i = do
  msg wg (MsgMoveAck (MoveAck (Right (w,i))))

-- | Send the scores to both players.
msgScores :: WebGame -> IO ()
msgScores wg = do
  let pl1 = player1 (theGame wg)
      pl2 = player2 (theGame wg)
      s1  = Score { theTurn = P1, theScore = score pl1 }
      s2  = Score { theTurn = P2, theScore = score pl2 }
  msg wg (MsgScore (s1,s2))

-- | Send the rack to the player identified by the Turn parameter.
sendRack :: WebGame -> Turn -> IO ()
sendRack wg t = do
  let pf = if t == P1 then player1 else player2
      r  = rack (pf (theGame wg))
  msgOne wg t (MsgRack r)

-- | Send the rack to the player identified by the Turn parameter.
sendRackOpponent :: WebGame -> IO ()
sendRackOpponent wg = do
  let t = if turn (theGame wg) == P1 then P2 else P1
  sendRack wg t

-- | Send name and rack to both players in a new game.
sendJoinAcks :: WebGame -> IO ()
sendJoinAcks wg = do
  let pl1 = player1 (theGame wg)
      pl2 = player2 (theGame wg)
      ja1 = MsgJoinAck (JoinAck { jaName = fst (p1 wg)
                                , jaRack = rack pl1
                                , jaTurn = P1
                                , jaOppName = fst (p2 wg)})
      ja2 = MsgJoinAck (JoinAck { jaName = fst (p2 wg)
                                , jaRack = rack pl2
                                , jaTurn = P2
                                , jaOppName = fst (p1 wg)})
  msgCurrent wg ja1
  msgOpponent wg ja2

-- | Send hints to the player identified by the Turn parameter.
sendHints :: WebGame -> Turn -> Maybe [Word]  -> IO ()
sendHints wg t ws = msgOne wg t (MsgHint ws)
