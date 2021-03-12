{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScrabbleWeb.Types
  ( Move(..)
  , MoveResponse(..)
  , Msg(..)
  , Client
  , WebGame(..)
  , Game(..)
  , Turn(..)
  , Evaluator(..)
  , Score(..) )
  where

import Data.Aeson
import GHC.Generics
import qualified Network.WebSockets as WS
import Data.Text (Text)
import Scrabble.Types
  ( WordPut
  , Rack
  , Letter(..)
  , Turn(..)
  , Game(..)
  , Evaluator(..))

-- ======== Types for ScrabbleWeb ========== --

-- | The name of the player and the WebSocket connection.
type Client = (Text, WS.Connection)

-- | A game of Scrabble and two clients.
data WebGame = WebGame
  { p1 :: Client
  , p2 :: Client
  , theGame :: Game
  } 

instance Eq WebGame where 
  g1 == g2 = fst (p1 g1) == fst (p1 g2) && fst (p2 g1) == fst (p2 g2)

-- | A move.
newtype Move = Move
  { word   :: WordPut }
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | The response to a move -- Either the score or an error message.
newtype MoveResponse = MoveResponse (Either String Int)
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | A score and a player, identified by the turn.
data Score = Score
  { theTurn :: Turn
  , theScore :: Int }
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | The protocol for communication between the server and clients.
data Msg =
    MsgJoin Text            -- ^ CLIENT  -> SERV   Client joins a game
  | MsgMove Move            -- ^ CLIENT <-> SERV   A client's move
  | MsgHint (Maybe [Word])  -- ^ CLIENT <-> SERV   Ask for/receive hints
  | MsgPass                 -- ^ CLIENT  -> SERV   Client passes move
  | MsgSwap [Letter]        -- ^ CLIENT  -> SERV   Letters to swap
  | MsgAnnounce Text        -- ^ SERV    -> CLIENT An announcement
  | MsgRack Rack            -- ^ SERV    -> CLIENT Send rack to client
  | MsgMoveRsp MoveResponse -- ^ SERV    -> CLIENT Was the move acceptable? 
  | MsgScore (Score, Score) -- ^ SERV    -> CLIENT The scores of both players
         deriving ( Show, Read, Generic, FromJSON, ToJSON )
