{-# LANGUAGE OverloadedStrings #-}
module ScrabbleWeb.Game
  ( gameStarter
  , playGame
  , newGame
  , aiGame )
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List (find)
import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan
import Control.Lens ((^.),(.~),(&))
import System.Log.Logger (infoM, errorM)
import Data.Aeson
import System.Random (getStdGen)
import ScrabbleWeb.Types
  ( WebGame(..)
  , p1
  , p2
  , theGame
  , Client
  , Msg(..)
  , Move(..)
  , MoveAck(..)
  , Move(..))
import Scrabble.Types
  ( Evaluator(..)
  , Game
  , turn, score, player1, player2, isAI, gameOver
  , Turn(..)
  , rack, name
  , Letter)
import Scrabble.Lang.Dict (englishDictionary)
import Scrabble.Lang.Search (makeWords)
import qualified Scrabble.Game.Game as G
  ( move
  , getPlayer
  , newGame
  , pass
  , swap )
import Scrabble.Game.AI (moveAI, newGame1P)
import Scrabble.Game.Validation
  ( valGameRules
  , valGameRulesAndDict )
import ScrabbleWeb.Announce
  ( announce
  , msgTurn
  , sendRackOpponent
  , sendJoinAcks
  , msgCurrent
  , msgOpponent
  , msgMoveAck
  , msgEog )
import Data.Functor (($>))

-- ========== Playing a game on the web ================ --

-- | Watch the Channel of connections and start a game in a
--   new thread when there are two clients waiting.
gameStarter :: BoundedChan Client -> IO ()
gameStarter state = loop
  where loop = do
          (n1,c1) <- readChan state
          (n2,c2) <- readChan state
          let (n1',n2') = distinctNames (n1,n2)
          infoM "Scrabble.Game" ("Starting game for "<>T.unpack n1'
                                  <> " and " <> T.unpack n2')
          d <- englishDictionary
          theGen <- getStdGen
          let ig = G.newGame n1' n2' theGen d
          (forkIO $ playGame (newGame (n1',c1) (n2',c2) ig)) >> loop

distinctNames :: (Text,Text) -> (Text,Text)
distinctNames (n1,n2) =
  (n1, fromJust $ find (/=n1) (n2 : map ((\n i -> n <> T.pack (show (i :: Int))) n2) [1..]))
  
newGame :: Client -> Client -> Game -> WebGame
newGame = WebGame

playGame :: WebGame -> IO ()
playGame wg = sendJoinAcks wg >> takeTurn wg $> ()

-- | Start a new game against the computer.
aiGame :: Client -> IO ()
aiGame (n,conn) = do
  infoM "Scrabble.Game" ("AI game for "<>T.unpack n)
  theGen <- getStdGen
  d      <- englishDictionary
  let ig = newGame1P n theGen d
  playGame (newGame (n,conn) ("Haskell",conn) ig) $> ()

-- | Take a turn.
takeTurn :: WebGame    -- ^ The game
         -> IO WebGame
takeTurn wg = do
     msgTurn wg
     -- msgScores wg
     let g = wg ^. theGame
     if g ^. gameOver
       then doGameOver wg
       else if g ^. (G.getPlayer g . isAI)
            then takeTurnAI wg
            else takeTurnManual wg

-- | Allow the computer to take a turn.
takeTurnAI :: WebGame -> IO WebGame
takeTurnAI wg = do
  let g = wg ^. theGame
  case moveAI g of
    Ev (Right (g',mv)) -> do
      msgMoveAck wg mv
      let wg' = wg & theGame .~ g' 
      sendRackOpponent wg'
      takeTurn wg'
    Ev (Left e)       -> do
      errorM "Scrabble.Game.takeTurnAI" (T.unpack e)
      announce wg e $> wg

-- | Take a turn manually.
takeTurnManual :: WebGame -> IO WebGame
takeTurnManual wg = do
  o <- decode <$> WS.receiveData (snd $ getClient wg)
  infoM "Scrabble.Game.takeTurnManual" ("[MSG] " <> show o)
  case o of
    Nothing  -> takeTurnManual wg
    Just msg -> case msg of
      MsgHint _         -> doHints wg >> takeTurn wg 
      MsgPass           -> doPass wg >>= takeTurn 
      MsgSwap w         -> doSwap wg w >>= takeTurn 
      MsgMove (Move wp bs) -> do
        case G.move valGameRules (wg ^. theGame) wp bs of
          Ev (Left e)        -> do msgCurrent wg (MsgMoveAck (MoveAck (Left e)))
                                   takeTurn wg 
          Ev (Right (g',mv)) -> do msgMoveAck wg mv
                                   let wg' = wg & theGame .~ g'
                                   sendRackOpponent wg'
                                   takeTurn wg'
      _                 -> takeTurn wg 

-- | Handle the situation when the game ends.
doGameOver :: WebGame -> IO WebGame
doGameOver wg = do
  let g      = wg ^. theGame
      pl1    = g ^. player1
      pl2    = g ^. player2
      draw   = pl1 ^. score == pl2 ^. score 
      winner = if pl1 ^. score > pl2 ^. score
               then pl1 else pl2
  infoM "Scrabble.Game" ("[End game] "<>T.unpack (pl1 ^. name)
                          <>" : "<> T.unpack (pl2 ^. name))
  msgEog wg
  if draw
    then announce wg "It's a draw!" $> wg
    else announce wg ("Congratulations " <> winner ^. name) $> wg

-- | Send hints to a player.
doHints :: WebGame -> IO ()
doHints wg = do
  let g  = wg ^. theGame
      w  = g ^. (G.getPlayer g . rack)
      hs = makeWords g w
  msgCurrent wg (MsgHint (Just hs))

-- | Let the player take a move by passing.
doPass :: WebGame -> IO WebGame
doPass wg = do
  let g = wg ^. theGame
  msgOpponent wg (MsgAnnounce "Opponent passed.")
  case G.pass g of
    Ev (Right g') -> pure (wg & theGame .~ g')
    Ev (Left e)   -> errorM "Scrabble.Game.doPass" (T.unpack e) $> wg

-- | Let the player take a move by swapping some tiles.
doSwap :: WebGame -> [Letter] -> IO WebGame
doSwap wg ls = do
  let g = wg ^. theGame
  case G.swap ls g of
    Ev (Right g') -> do let wg' = wg & theGame .~ g'
                        sendRackOpponent wg'
                        announce wg' "Swapped tiles"
                        pure wg'
    Ev (Left e)   -> errorM "Scrabble.Game.doSwap" (T.unpack e) $> wg

-- | Get client whose turn it is. 
getClient :: WebGame -> Client
getClient wg = if wg ^. (theGame . turn) == P1 then wg ^. p1 else wg ^. p2
