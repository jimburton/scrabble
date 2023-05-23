-- |
-- Module      : Scrabble.Announce
-- Description : Functions for sending messages ("announcements") to players connected
--               to the game server.
-- Maintainer  : jimburton1@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- 
module ScrabbleWeb.Announce
  ( announce
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

import Prelude hiding (Word)
import Control.Lens ((^.))
import Control.Monad ( unless )
import qualified Network.WebSockets as WS
import Data.Aeson ( encode )
import Data.Text (Text)
import Scrabble.Types
  ( Player
  , rack
  , player1
  , player2
  , turn
  , score
  , isAI
  , Word
  , MoveResult(..))
import ScrabbleWeb.Types
  ( WebGame
  , p1
  , p2
  , theGame
  , Turn(..)
  , Msg(..)
  , Score(..)
  , JoinAck(..)
  , MoveAck(..)
  , Client)

-- * Sending messages to clients.

-- | Send a message if the player is not AI
send :: Player -> Client -> Msg -> IO ()
send p (_,conn) m =
  let o = encode m in
    (unless (p ^. isAI) $ WS.sendTextData conn o)

-- | Send a message to both players.
msg :: WebGame -> Msg -> IO ()
msg wg m = do
  send (wg ^. (theGame . player1)) (wg ^. p1) m
  send (wg ^. (theGame . player2)) (wg ^. p2) m

-- | Send a message to one player, identified by the Turn parameter.
msgOne :: WebGame -> Turn -> Msg -> IO ()
msgOne wg t m = do
  let (pl,cl) = if t == P1
                then (wg ^. (theGame . player1), wg ^. p1)
                else (wg ^. (theGame . player2), wg ^. p2)
  send pl cl m

-- | Send a message to the player whose turn it currently is.
msgCurrent :: WebGame -> Msg -> IO ()
msgCurrent wg = msgOne wg (wg ^. (theGame . turn))

-- | Send a messsage to the player whose turn it currently is not.
msgOpponent :: WebGame -> Msg -> IO ()
msgOpponent wg = msgOne wg (other (wg ^. (theGame . turn)))

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
msgTurn wg = msg wg (MsgTurn $ wg ^. (theGame . turn))

-- | Acknowledge to a legal move, sending the move to both players.
msgMoveAck :: WebGame    -- ^ The webgame.
           -> MoveResult -- ^ The move that was played.
           -> IO ()
msgMoveAck wg mv = msg wg (MsgMoveAck (MoveAck (Right mv)))

-- | Send the scores to both players.
{-msgScores :: WebGame -> IO ()
msgScores wg = do
  let ss = getScores wg
  msg wg (MsgScore ss) 
-}
-- | Send the End of Game message to both players
msgEog :: WebGame -> IO ()
msgEog wg = do
  let ss = getScores wg
  msg wg (MsgEog ss)

-- Get the scores of both players.
getScores :: WebGame -> (Score,Score)
getScores wg =
  let pl1 = wg ^. (theGame . player1)
      pl2 = wg ^. (theGame . player2)
      s1  = Score { theTurn = P1, theScore = pl1 ^. score }
      s2  = Score { theTurn = P2, theScore = pl2 ^. score } in
  (s1,s2)

-- | Send the rack to the player identified by the Turn parameter.
sendRack :: WebGame -> Turn -> IO ()
sendRack wg t = do
  let pf = if t == P1 then player1 else player2
      r  = wg ^. (theGame . pf . rack)
  msgOne wg t (MsgRack r)

-- | Send the rack to the player identified by the Turn parameter.
sendRackOpponent :: WebGame -> IO ()
sendRackOpponent wg = do
  let t = if wg ^. (theGame . turn) == P1 then P2 else P1
  sendRack wg t

-- | Send name and rack to both players in a new game.
sendJoinAcks :: WebGame -> IO ()
sendJoinAcks wg = do
  let pl1 = wg ^. (theGame . player1)
      pl2 = wg ^. (theGame . player2)
      ja1 = MsgJoinAck (JoinAck { jaName = fst (wg ^. p1)
                                , jaRack = pl1 ^. rack
                                , jaTurn = P1
                                , jaOppName = fst (wg ^. p2)})
      ja2 = MsgJoinAck (JoinAck { jaName = fst (wg ^. p2)
                                , jaRack = pl2 ^. rack
                                , jaTurn = P2
                                , jaOppName = fst (wg ^. p1)})
  msgCurrent wg ja1
  msgOpponent wg ja2

-- | Send hints to the player identified by the Turn parameter.
sendHints :: WebGame -> Turn -> Maybe [Word]  -> IO ()
sendHints wg t ws = msgOne wg t (MsgHint ws)
