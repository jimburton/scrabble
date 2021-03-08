{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScrabbleWeb.Types
  ( Move(..)
  , OpponentMove(..)
  , MoveResponse(..)
  , Msg(..)
  , Client
  , ServerState
  , WebGame(..)
  , Game(..))
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
  , Game(..))

-- ======== Types for ScrabbleWeb ========== --
data Move = Move
  { word   :: WordPut
  , rack   :: Rack
  , player :: Turn }
  deriving ( Show, Generic, FromJSON, ToJSON )

data OpponentMove = OpponentMove
  { wordPlayed :: WordPut }
  deriving ( Show, Generic, FromJSON, ToJSON )

-- | The response to a move -- Either the score or an error message.
data MoveResponse = MoveResponse (Either String Int)
  deriving ( Show, Generic, FromJSON, ToJSON )

data Msg = MsgAnnounce Text
         | MsgMove Move
         | MsgOppMove OpponentMove
         | MsgMoveRsp MoveResponse
  deriving ( Show, Generic, FromJSON, ToJSON )

type Client = (Text, WS.Connection)

type ServerState = [WebGame]

data WebGame = WebGame
               { p1 :: Client
               , p2 :: Client
               , theGame :: Game
               } 

instance Eq WebGame where 
  g1 == g2 = fst (p1 g1) == fst (p1 g1) && fst (p2 g1) == fst (p2 g1)
