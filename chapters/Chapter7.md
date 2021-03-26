# Chapter Six: The scrabble server

[Contents](../README.md)

Going a step beyond writing clients in Haskell that import the library
directly, in this chapter we develop a server for remote
clients. These can be written in any language so long as they can
reach the server, send JSON objects representing requests to join a
game, moves and so on. In this way the library is fully decoupled from
clients, and we demonstrate this by writing a web-based client using
HTML, CSS and Javascript.

**SOURCE TREE**

## Serving websockets

We will write the server using *websockets*. This is a modern protocol
that allows full duplex communications (either side can send a
message) over TCP between clients and servers on a network such as the
web. It is designed with web browsers in mind and it's easy to
communicate over websockets with Javascript, but any language that has
a library for the protocol can make a connection. Messages are
normally sent back and forth over the standard HTTP or HTTPS ports,
meaning that there aren't likely to be firewall issues (we'll run our
server on a higher port during development, as you need admin
privileges to attach to ports below 1024). The handshake between
client and server starts with a HTTP request/response. After that,
communication switches to a much more efficient binary protocol.

To demonstrate how easy it is to get started, here is a self-contained
"echo" websocket server.

```haskell
{-# Language OverloadedStrings #-}
module Main
  where

import Control.Monad (forever)
import qualified Network.WebSockets as WS
import Data.Text (Text) 

main :: IO ()
main = do
  putStrLn "starting echo on port 9160"
  WS.runServer "127.0.0.1" 9160 wsapp

wsapp :: WS.ServerApp
wsapp pending = WS.acceptRequest pending >>= echo

echo :: WS.Connection -> IO ()
echo conn = forever $ do
  msg <- WS.receiveData conn
  WS.sendTextData conn $ doEcho msg

doEcho :: Text -> Text
doEcho msg = "echo: " <> msg
```

The `main` function starts the websocket server `wsapp` running on the
loopback address 127.0.0.1 and the port 9160. This continually accepts
connections and passes them in their own thread to the `echo`
action. Each of these threads loops forever, waiting to receive a
message from the client then sending one back.

Here is a client that runs in a browser.

```html
<!DOCTYPE html>
<html>
    <head>
    <script type="text/javascript">
    function Client(socket) {
	socket.onopen = function () {
	}
	socket.onclose = function () {
	    alert("closed web socket");
	}
	socket.onerror = function (event) {
	    alert(event);
	}
	socket.onmessage = function (event) {
	    alert(event.data);
	}
    }
    var client;
    var socket;
    // Connect to the websocket server.
    function connect() {
	socket = new WebSocket("ws://localhost:9160/")
	client = new Client(socket);
    }
    </script>
    </head>
    <body onload="connect()">
      <label for="echoText">Text to echo:</label><br>
      <input type="text" id="echoText" name="echoText"><br>
      <button onclick="socket.send(document.getElementById('echoText').value)">Echo</button> 
    </body>
</html>
```
When the page is loaded the `connect` function runs. This creates a `WebSocket`
object connected to the server. It uses the socket to create a `Client` object
that defines event handlers for the lifecycle of the socket. The `onmessage`
handler is called whenever a message comes in from the server. The form in the 
body of the page allows us to send messages to the server using `socket.send`.

To write a server that allows users to play Scrabble, we can start with code like
this. On the server side we need to replace `echo` with code that parses messages
from the user and acts accordingly. Similarly, on the client side we need to add
code to the `onmessage` handler that parses input from the server and uses it to 
present the game.

The format we will use for the communication is JSON (Javascript
Object Notation), which is nice and simple, human-readable and well
supported in Javascript and Haskell. In fact, JSON is a subset of
Javascript, so there's very little parsing to do on the client side. The
parts of Javascript that are allowed in JSON are *numbers*, *strings*,
*booleans*, *objects* (braces containing
comma-separated key:value pairs, like `{"name":"James", "score": 42}`
and *arrays* (comma-separated values within square brackets).

We do have to turn Javascript objects into strings and back again
though. We do this with `JSON.stringify` and `JSON.parse`.

```javascript
// send a JSON object (an array of numbers) to the server
socket.send(JSON.stringify([1, 2, 3, 4]));

socket.onmessage = function (event) {
   // parse the message as JSON
   var d = JSON.parse(event.data);
	....
}
```

Thanks to the `aeson` library, it isn't that much more difficult on
the Haskell side. `aeson` is a powerful and neat way of converting
Haskell values into JSON representations and back again. 

Datatypes that need to be sent over the network are made into
instances of the `ToJSON` and `FromJSON` typeclasses. We can do this
ourselves by defining `encode` and `decode` functions for each type,
but as we don't want to do anything special the instances can be
derived. In order to make this work we have to turn on two language
extensions `DeriveGeneric` and `DeriveAnyClass`. For each type we want
to derive the `aeson` instances for, we also need to derive an instance
of `Generic`, which is a typeclass the compiler uses internally. 

We will keep the data sent back and forth to an absolute
minimum. There's no need to send entire boards, for instance. Clients
can start with an empty board and add words to it one by one. The
`MoveResult` datatype will serve that purpose:

```haskell
-- | The Record returned by move functions.
data MoveResult = MoveResult
                  { mrWord            :: WordPut -- ^ The word that was played.
                  , mrAdditionalWords :: [Word]  -- ^ The additional words.
                  , mrBlanks          :: [Int]   -- ^ The positions in the word that were blank.
                  , mrScore           :: Int     -- ^ The score.
                  }
                deriving (Show, Read, Eq)
```
To make instances of `FromJSON` and `ToJSON` for `MoveResult`, there must 
already be instances for every type contained in it. Instances exist for
all basic datatypes such as `Int` and so on, but not for our type `Letter`, 
which is part of `Word`, so we need to know how to serialise that too. We will
also want to let clients know whose turn it is, so we will derive instances for
`Turn`.

The normal place to say that we want the compiler to derive typeclass
instances for our types is immediately after their definition, so that
would be in `src/Scrabble/Types.hs`. However, we don't want to add the
`aeson` dependency to the library. It doesn't know anything about how
clients might be implemented and we want to to keep it that way.

There is a language extension that does what we need, `StandaloneDeriving`. By 
turning this on we can add the deriving clauses to `ScrabbleWeb.Types`, where
it does make sense to have the `aeson` dependency.

```
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

-- ==== Make types from Scrabble.Types serialisable in JSON ========== --

deriving instance Generic  MoveResult
deriving instance FromJSON MoveResult
deriving instance ToJSON   MoveResult

deriving instance Generic  Letter
deriving instance FromJSON Letter
deriving instance ToJSON   Letter

deriving instance Generic  Turn
deriving instance FromJSON Turn
deriving instance ToJSON   Turn
```

Now we can turn a `Turn`, a `Letter` or a `MoveResult` into JSON with `encode`.
We can read text into `Maybe`s of those type with `decode`.
	
```
$ cabal repl scrabble-server
> :m + Scrabble
> :m + ScrabbleWeb.Types
> :m + Data.Aeson
> let w = [((0,0),(C,3)),((0,1),(A,1)),((0,2),(T,1))]
> let bs = [[M,A,T]]
> encode (MoveResult w bs [] 42)
"{"mrScore":42,"mrWord":[[[0,0],["C",3]],[[0,1],["A",1]],[[0,2],["T",1]]],"mrAdditionalWords":[["M","A","T"]],"mrBlanks":[]}"
> decode it :: Maybe MoveResult
Just
    ( MoveResult
        { mrWord =
            [
                (
                    ( 0
                    , 0
                    )
                ,
                    ( C
                    , 3
                    )
                )
            ,
                (
                    ( 0
                    , 1
                    )
                ,
                    ( A
                    , 1
                    )
                )
            ,
                (
                    ( 0
                    , 2
                    )
                ,
                    ( T
                    , 1
                    )
                )
            ]
        , mrAdditionalWords =
            [
                [ C
                , O
                , W
                ]
            ]
        , mrBlanks = []
        , mrScore = 42
        }
    )
``` 

As you can see, a record is encoded into a JSON object with key:value
pairs for the fields.  Algebraic datatypes (i.e. non-records) are
encoded as an object with a key called "tag" with the name of the type
in it and a key called "contents" with the data. For example, here is
a serialised value of the `Msg` type, one we'll come to later in the
chapter.

```haskell
> encode (MsgTurn P1)
"{"contents":"P1","tag":"MsgTurn"}"
```
`aeson` is a great library.  -- for the full detail on how it works, [read the
tutorial](https://artyom.me/aeson).



## A (much) nicer UI that allows proper remote multiplayer functionality

### The server
  + 	
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


[Contents](../README.md) | [Chapter Eight](Chapter8.md)
