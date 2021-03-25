module ScrabbleWeb.Announce
  ( announce
  , msgScores
  , msgOpponent
  , msgCurrent
  , msgMoveAck
  , msgTurn
  , msgEog
  , maybeAnnounce
  , sendRack
  , sendRackOpponent
  , sendHints
  , sendJoinAcks )
  where

import Debug.Trace
import Prelude hiding (Word)
import qualified Network.WebSockets as WS
import Data.Aeson
import Data.Text (Text)
import Scrabble.Types
  ( Player
  , rack
  , Game
  , player1
  , player2
  , turn
  , score
  , isAI
  , Word
  , WordPut
  , MoveResult(..))
import ScrabbleWeb.Types
  ( WebGame(..)
  , Turn(..)
  , Msg(..)
  , Score(..)
  , JoinAck(..)
  , MoveAck(..)
  , Client)
import Lens.Simple((^.))
  
-- ====== Sending messages to clients =========== --

-- | Send a message if the player is not AI
send :: Player -> Client -> Msg -> IO ()
send p (_,conn) m =
  let o = encode m in
    if not (p ^. isAI) then WS.sendTextData conn o else pure ()

-- | Send a message to both players.
msg :: WebGame -> Msg -> IO ()
msg wg m = do
  send (theGame wg ^. player1) (p1 wg) m
  send (theGame wg ^. player2) (p2 wg) m

-- | Send a message to one player, identified by the Turn parameter.
msgOne :: WebGame -> Turn -> Msg -> IO ()
msgOne wg t m = do
  let (pl,cl) = if t == P1 then ((theGame wg) ^. player1,p1 wg) else ((theGame wg) ^. player2,p2 wg)
  send pl cl m

-- | Send a message to the player whose turn it currently is.
msgCurrent :: WebGame -> Msg -> IO ()
msgCurrent wg = msgOne wg ((theGame wg) ^. turn) 

-- | Send a messsage to the player whose turn it currently is not.
msgOpponent :: WebGame -> Msg -> IO ()
msgOpponent wg = msgOne wg (other (theGame wg ^. turn))

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
msgTurn wg = msg wg (MsgTurn $ theGame wg ^. turn)

-- | Acknowledge to a legal move, sending the move to both players.
msgMoveAck :: WebGame    -- ^ The webgame.
           -> MoveResult -- ^ The move that was played.
           -> IO ()
msgMoveAck wg mv = msg wg (MsgMoveAck (MoveAck (Right mv)))

-- | Send the scores to both players.
msgScores :: WebGame -> IO ()
msgScores wg = do
  let ss = getScores wg
  msg wg (MsgScore ss)

-- | Send the End of Game message to both players
msgEog :: WebGame -> IO ()
msgEog wg = do
  let ss = getScores wg
  msg wg (MsgEog ss)

-- Get the scores of both players.
getScores :: WebGame -> (Score,Score)
getScores wg =
  let pl1 = theGame wg ^. player1
      pl2 = theGame wg ^. player2
      s1  = Score { theTurn = P1, theScore = pl1 ^. score }
      s2  = Score { theTurn = P2, theScore = pl2 ^. score } in
  (s1,s2)

-- | Send the rack to the player identified by the Turn parameter.
sendRack :: WebGame -> Turn -> IO ()
sendRack wg t = do
  let pf = if t == P1 then player1 else player2
      r  = theGame wg ^. (pf . rack)
  msgOne wg t (MsgRack r)

-- | Send the rack to the player identified by the Turn parameter.
sendRackOpponent :: WebGame -> IO ()
sendRackOpponent wg = do
  let t = if theGame wg ^. turn == P1 then P2 else P1
  sendRack wg t

-- | Send name and rack to both players in a new game.
sendJoinAcks :: WebGame -> IO ()
sendJoinAcks wg = do
  let pl1 = theGame wg ^. player1
      pl2 = theGame wg ^. player2
      ja1 = MsgJoinAck (JoinAck { jaName = fst (p1 wg)
                                , jaRack = pl1 ^. rack
                                , jaTurn = P1
                                , jaOppName = fst (p2 wg)})
      ja2 = MsgJoinAck (JoinAck { jaName = fst (p2 wg)
                                , jaRack = pl2 ^. rack
                                , jaTurn = P2
                                , jaOppName = fst (p1 wg)})
  msgCurrent wg ja1
  msgOpponent wg ja2

-- | Send hints to the player identified by the Turn parameter.
sendHints :: WebGame -> Turn -> Maybe [Word]  -> IO ()
sendHints wg t ws = msgOne wg t (MsgHint ws)
