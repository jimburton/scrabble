{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, TemplateHaskell #-}
module ScrabbleWeb.Types
  ( Move(..)
  , MoveAck(..)
  , Msg(..)
  , Client
  , WebGame(..)
  , p1, p2, theGame -- lenses for WebGame
  , Turn(..)
  , Evaluator(..)
  , Score(..)
  , JoinAck(..)
  , Conf(..))
  where

import Prelude hiding (Word)
import System.Log (Priority)
import Data.Aeson
import GHC.Generics
import qualified Network.WebSockets as WS
import Data.Text (Text)
import Lens.Simple
import Scrabble.Types
  ( WordPut
  , Rack
  , Letter(..)
  , Turn(..)
  , Game
  , Evaluator(..)
  , Word
  , MoveResult(..)) 

-- ======== Types for ScrabbleWeb ========== --

-- ==== Make types from Scrabble.Types serialisable in JSON ========== --
-- Make MakeResult serialisable in JSON

deriving instance Generic  MoveResult
deriving instance FromJSON MoveResult
deriving instance ToJSON   MoveResult

deriving instance Generic  Letter
deriving instance FromJSON Letter
deriving instance ToJSON   Letter

deriving instance Generic  Turn
deriving instance FromJSON Turn
deriving instance ToJSON   Turn

-- | Container for the config.
data Conf = Conf
  { hostname     :: Text
  , port         :: Int
  , log_file     :: Text
  , log_priority :: Priority
  } deriving Show

-- | The name of the player and the WebSocket connection.
type Client = (Text, WS.Connection)

-- | A game of Scrabble and two clients.
data WebGame = WebGame
  { _p1 :: Client
  , _p2 :: Client
  , _theGame :: Game
  }
-- | Make lenses for WebGame
$(makeLenses ''WebGame)


--instance Eq WebGame where 
--  g1 == g2 = fst (p1 g1) == fst (p1 g2) && fst (p2 g1) == fst (p2 g2)

-- | Acknowledging the player into a game
data JoinAck = JoinAck { jaName :: Text
                       , jaRack :: Rack
                       , jaTurn :: Turn
                       , jaOppName :: Text }
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | A move.
data Move = Move
  { word   :: WordPut
  , blanks :: [Int] }
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | The response to a move -- Either an error message or
--   a pair of the WordPut and a triple of the additional words,
--   indices of blanks, and score.
newtype MoveAck = MoveAck ( Either Text -- The error message.
                            MoveResult) -- The result.
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | A score and a player, identified by the turn.
data Score = Score
  { theTurn :: Turn
  , theScore :: Int }
  deriving ( Show, Read, Generic, FromJSON, ToJSON )

-- | The protocol for communication between the server and clients.
data Msg =
    MsgJoin (Text,Bool)     -- ^ CLIENT  -> SERV   Client (Name, isAi) joins a game.
  | MsgJoinAck JoinAck      -- ^ CLIENT <-  SERV   Client is accepted into a game.
  | MsgTurn Turn            -- ^ CLIENT <-  SERV   Send the current turn.
  | MsgMove Move            -- ^ CLIENT <-> SERV   A client's move.
  | MsgHint (Maybe [Word])  -- ^ CLIENT <-> SERV   Ask for/receive hints.
  | MsgPass                 -- ^ CLIENT  -> SERV   Client passes move.
  | MsgSwap [Letter]        -- ^ CLIENT  -> SERV   Letters to swap.
  | MsgAnnounce Text        -- ^ CLIENT <-  SERV   A general purpose announcement.
  | MsgRack Rack            -- ^ CLIENT <-  SERV   Send rack to client.
  | MsgMoveAck MoveAck      -- ^ CLIENT <-  SERV   Was the move acceptable? If so, score and word. 
 -- | MsgScore (Score, Score) -- ^ CLIENT <-  SERV   The scores of both players.
  | MsgEog (Score,Score)    -- ^ CLIENT <-  SERV   End of game.
         deriving ( Show, Read, Generic, FromJSON, ToJSON )
