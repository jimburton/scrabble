# Chapter Six: The web client

## A (much) nicer UI that allows proper remote multiplayer functionality

+ **`v-0.6`**:
  + A web interface is added, using websockets and the `aeson` library
    to handle encoding and decoding JSON, and a client written using HTML and JavaScript. 

### The server
  + The `aeson` library is a
    powerful and neat way of converting Haskell values into JSON
    representations and back again, and this is how we will send data
    over the network. Datatypes that need to be sent over the network
    are made into instances of `ToJSON` and `FromJSON`. Fortunately, since we don't
	want to do anything special these instances can be derived. For instance,
    the definition of the `Player` datatype is now:
	
	```
	-- | A player has a name, a rack and a score, and is either an interactive or an AI player.
    data Player = Player {
      name  :: Text
    , rack  :: Rack
    , score :: Int
    , isAI  :: Bool
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

	```
	To make this work we need to turn on some GHC extensions: `DeriveGeneric` and `DeriveAnyClass`. That
	means adding a language pragma to the top of any file with this kind of definition in it:
	
	```
	{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
	```
	
	and adding the extensions to the `cabal` config file. Now we can turn a `Player` into JSON and 
	read some JSON into a `Player` object:
	
	```
	> :m + Scrabble.Types
    > :m + Data.Aeson
    > encode (Player {name="James", rack=[A, B, C], score=42, isAI=False})
    "{\"name\":\"James\",\"rack\":[\"A\",\"B\",\"C\"],\"score\":42,\"isAI\":false}"
	> decode it :: Maybe Player
    Just
        ( Player
            { name = "James"
            , rack =
                [ A
                , B
                , C
                ]
            , score = 42
            , isAI = False
            }
        )
	```
	`Aeson` is great -- to find out how it works, [read the tutorial](https://artyom.me/aeson).
	
  + We need to write a server that webclients can connect to, and that
	can interact with the `Scrabble` library.  It consists of a
	`WebSocket` server, allowing duplex communication (either side can
	send and receive messages) and taking advantage of the capability
	of modern browsers to keep connections alive by sending pings at
	regular intervals.
	
	This code lives in `web/server` and the entry point is in `Main.hs`. A new stanza is added
	to the `cabal` config file, and you can now run `cabal run scrabble-server`.
	
	Study the code in `web/server/`. The server works as follows:
	
	+ A number of new types are declared in `ScrabbleWeb.Types`. Most
	  of these are types that will be sent over the network so they
	  are instance of `ToJSON` and `FromJSON`.  The `WebGame` type provides
	  a wrapper around the `Game` type, because we now need to maintain two `Client`
	  values alongside the game. 
	  
	  The heart of the whole thing is the `Msg` type, which forms the
	  protocol of the game. Every JSON object that is sent between
	  clients and the server will be a `Msg` of some kind:
	  
	  ```
	  data Msg = MsgAnnounce Text  -- ^ SERV    -> CLIENT An announcement
         | MsgRack Rack            -- ^ SERV    -> CLIENT Send rack to client
         | MsgJoin Text            -- ^ CLIENT  -> SERV   Client joins a game
         | MsgMove Move            -- ^ CLIENT <-> SERV   A client's move
         | MsgHint (Maybe [Word])  -- ^ CLIENT <-> SERV   Ask for/receive hints
         | MsgPass                 -- ^ CLIENT  -> SERV   Client passes move
         | MsgSwap [Letter]        -- ^ CLIENT  -> SERV   Letters to swap
         | MsgMoveRsp MoveResponse -- ^ SERV    -> CLIENT Was the move acceptable? 
         | MsgScore (Score, Score) -- ^ SERV    -> CLIENT The scores of both players
       deriving ( Show, Read, Generic, FromJSON, ToJSON )
	  ```
	
	+ In the `Main` module, the `main` function begins by creating a
	  new `BoundedChan`, which is a bounded concurrent queue that can
	  contain at most two `Client` values. A `Client` consists of a
	  name and a socket connection.
   +  The `main` function then forks a new thread that runs an action
      called `gameStarter` forever. This action watches the `BoundedChan`, and when it
	  has two clients in it,  forks a new thread in which the game for the two players
	  will be played. 
   +  The final thing the `main` function does is to run the WebSocket server. The server
      listens for connections on port `9160` at the address
      `127.0.0.1` (you can change thse settings in the `main`
      function). When new connections come in, the server adds them to the `BoundedChan`.
	  
   +  The functions that actually interact with library to play the
	  game are in the module `ScrabbleWeb.Game`. These are `playGame`,
	  `takeTurn`, `doSwap` and so on, and are are very similar to the
	  CLI version. The difference is that the methods that take turns
	  need to read the move in from the websocket and decide what to
	  do based on the message received. This code is in the case
	  statement in the case statement in the `takeTurnManual`
	  function:
	  
	  ```
	  o <- decode <$> WS.receiveData (snd $ getClient wg)
      putStrLn ("Received: " ++ show o)
      case o of
        Nothing  -> takeTurnManual wg -- retake the turn with the same game
        Just msg -> case msg of
		  MsgHint           -> do sendHints wg -- send hints to current player
		                          takeTurnManual wg -- retake the turn with current player
		  MsgSwap tiles     -> swapTiles tiles >>= takeTurnManual
		  MsgPass           -> doPass >>= takeTurnManual -- let the next player have a go
          MsgMove (Move wp) -> do
            let g = theGame wg
                w = wordPutToText wp
            case G.move valGameRules g wp [] of
              Ev (Left e) -> do announce wg e
                                takeTurn wg $ Just (w  <> ": NO SCORE")
              Ev (Right (g',sc)) -> do msgOpponent wg msg
                                       takeTurn (wg { theGame = g' }) (Just (T.pack $ show sc))
	  ```
	  
	  The process of sending information back to clients is done using
	  functions in the module `ScrabbleWeb.Announce`.
	
### The client
    See `web/client`.


[Contents](../README.md) | [Chapter Seven](Chapter7.md)
