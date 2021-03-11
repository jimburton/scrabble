{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScrabbleWeb.Types
  ( Move(..)
  , OpponentMove(..)
  , MoveResponse(..)
  , Msg(..)
  , Client
  , ServerState
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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Scrabble.Types
  ( WordPut
  , Rack
  , Letter(..)
  , Turn(..)
  , Game(..)
  , Evaluator(..))

-- ======== Types for ScrabbleWeb ========== --

type Client = (Text, WS.Connection)

type ServerState = [Client]

data WebGame = WebGame
  { p1 :: Client
  , p2 :: Client
  , theGame :: Game
  } 

newtype Move = Move
  { word   :: WordPut }
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | The response to a move -- Either the score or an error message.
newtype MoveResponse = MoveResponse (Either String Int)
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

data Score = Score Turn Int
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

data Msg =
    MsgAnnounce Text        -- ^ SERV    -> CLIENT An announcement
  | MsgRack Rack            -- ^ SERV    -> CLIENT Send rack to client
  | MsgJoin Text            -- ^ CLIENT  -> SERV   Client joins a game
  | MsgMove Move            -- ^ CLIENT <-> SERV   A client's move
  | MsgHint (Maybe [Word])  -- ^ CLIENT <-> SERV   Ask for/receive hints
  | MsgPass                 -- ^ CLIENT  -> SERV   Client passes move
  | MsgSwap [Letter]        -- ^ CLIENT  -> SERV   Letters to swap
  | MsgMoveRsp MoveResponse -- ^ SERV    -> CLIENT Was the move acceptable? 
  | MsgScore (Score, Score) -- ^ SERV    -> CLIENT The scores of both players
         deriving ( Show, Read, Generic, FromJSON, ToJSON )

instance Eq WebGame where 
  g1 == g2 = fst (p1 g1) == fst (p1 g1) && fst (p2 g1) == fst (p2 g1)
