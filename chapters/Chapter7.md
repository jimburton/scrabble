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
that allows full duplex communications (either side can send a message
at any time) over TCP between clients and servers on a network such as
the web. It is designed with web browsers in mind and it's easy to
communicate over websockets with Javascript, but we can write servers
and clients in any language that has a library for the
protocol. Messages are normally sent back and forth over the standard
HTTP or HTTPS ports, meaning that there aren't likely to be firewall
issues. We'll run our server on a higher port during development, as
you need admin privileges to attach to ports below 1024. The handshake
between client and server starts with a HTTP request/response. After
that, communication switches to a much more efficient binary protocol.

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
loopback address 127.0.0.1 and the port 9160. The server continually
accepts connections and passes them in their own thread to the `echo`
action. Each of these new threads loops forever, waiting to receive a
message from the client then bouncing it back.

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

## Serialising data

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
the Haskell side. `aeson` provides a powerful and neat way of converting
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

There is a language extension that does what we need,
`StandaloneDeriving`. By turning this on we can add the deriving
clauses to `web/server/ScrabbleWeb/Types.hs`, which is nothing to do
with the library and where it does make sense to add the `aeson`
dependency.

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
a serialised value of the `Msg` type, one we'll come to later in this
chapter.

```haskell
> encode (MsgTurn P1)
"{"contents":"P1","tag":"MsgTurn"}"
```
`aeson` is a great library. If you need something other than the default
derived instances of your types (e.g. if there are some fields you don't 
want to serialise, or you want to modify them when you do) you can find
out how it all works by [reading the tutorial](https://artyom.me/aeson).

## The protocol

The protocol for our game consists of a definition of the data that
can be sent between client and server, the order in which it must be
sent in order for a game to be played, and the expectations that each side
can have about the behaviour of the other.

The lifecycle of an AI game will be something like the illustration
below, where the data associated with messages is shown in square brackets 
after its name. 

When a request to join a game is acknowledged, the server sends the
name the player has been assigned, the name of their opponent, their
first rack and their turn (`P1` or `P2`). In an AI game the human
player is `P1`.

```
 --------                                       --------
| CLIENT |                                     | SERVER |       
 --------                                       --------
     |      -- REQUEST FOR AI GAME [NAME] -->      |
	 |                                             |
	 |      <- ACK [NAME,OPPNAME,RACK,TURN] --     |
	 |                                             |
```

Now the game is in play the following sequence, with possible
variations explained below, is repeated until the game ends. The
server sends the client the `TURN` message to tell them it is waiting
for a move. If a legal move is received the server responds by sending
the word itself, the list of additional words that were created and the 
score. Then the server sends the new rack. The turn is passed to the other
player and the sequence is repeated (except that when it is the AI player's
turn no `MOVE` or `RACK` messages are sent of course).

```
	 |                                             |
	 |      <- TURN [P1]                           |
	 |                                             |
	 |      -- MOVE [MOVE] -->                     |
	 |                                             |
	 |      <- MOVE ACK [MOVE,P1] --               |
	 |                                             |
	 |      <- RACK [LETTERS] --                   |	 
```

If the client sends an illegal move then, rather than receiving `MOVE ACK` 
and a new rack, it receives a message explaining what was wrong with the
move and the `TURN` sequence is repeated without the turn being passed to 
the other player.

```
     |      -- MOVE [P1] -->                       |
	 |                                             |
	 |      <- ANNOUNCE [ERROR]                    |
	 |                                             |
	 |      <- TURN [P1]                           |
	 |                                             |

```
Any time it is the client's turn they can send `PASS MOVE`. The server
responds by giving the turn to P2.

```
     |      <- TURN [P1] --                        |
	 |                                             |
     |      -- PASS MOVE [P1] -->                  |
	 |                                             |
	 |      <- TURN [P2] --                        |
	 |                                             |

```

Similarly, if it the client's turn they can use it by swapping tiles.

```
     |      <- TURN [P1] --                        |
	 |                                             |
     |      -- SWAP [TILES] -->                    |
	 |                                             |
	 |      <- TURN [P2] --                        |
	 |                                             |
```

Finally, if it is the clients turn they can ask for hints.

```
     |      <- TURN [P1] --                        |
	 |                                             |
     |      -- HINT -->                            |
	 |                                             |
	 |      <- HINT [HINTS] --                     |
	 |                                             |
```

Whenever the client is expecting the `TURN` message it could
alternatively receive the news that the game is ended.

```			
	 |      <- END OF GAME [SCORES] --             |
	 
```

The lifecycle for a two-player game is similar. Only the client who
currently has the turn can submit a move in any of the three ways or
ask for hints. Only when they `PASS`, `SWAP` or submit a legal move
does the turn pass to the next player. The `TURN` and `MOVE ACK`
messages are sent to both clients, and they will use them to keep
their state up to date. At any stage the client might receive the `END
OF GAME` announcement.

On the Haskell side we make a datatype called `Msg` that embodies the
messages in the protocol. It needs to be serialisable to and from
JSON. Along with the comments, it acts as a kind of data dictionary
for the protocol.

```haskell
-- | The protocol for communication between the server and clients.
data Msg =
    MsgJoin (Text,Bool)     -- ^ CLIENT  -> SERV   Client (Name, isAi) joins a game.
  | MsgJoinAck JoinAck      -- ^ CLIENT <-  SERV   Client is accepted into a game.
  | MsgTurn Turn            -- ^ CLIENT <-  SERV   Send the current turn.
  | MsgMove Move            -- ^ CLIENT <-> SERV   A client's move.
  | MsgMoveAck MoveAck      -- ^ CLIENT <-  SERV   Was the move acceptable? If so, score and word. 
  | MsgHint (Maybe [Word])  -- ^ CLIENT <-> SERV   Ask for/receive hints.
  | MsgPass                 -- ^ CLIENT  -> SERV   Client passes move.
  | MsgSwap [Letter]        -- ^ CLIENT  -> SERV   Letters to swap.
  | MsgAnnounce Text        -- ^ CLIENT <-  SERV   A general purpose announcement.
  | MsgEog (Score,Score)    -- ^ CLIENT <-  SERV   End of game.
         deriving ( Show, Read, Generic, FromJSON, ToJSON )
```

## Writing the server

Every time a connection comes in to our server it could be from a
client that wants to play an AI game or from one that wants to play
against another human.  Requests for AI games can be served straight
away. Requests for games against humans need to be saved until there
are two of them. So we need to manage a *buffered queue*, a concurrent
data structure that can contain at most two clients. The main thread
will add incoming requests for non-AI games to the queue and a
separate thread, the `gameStarter` thread, will watch this queue,
waiting until there are two requests then creating a new game in a
separate thread and allowing them to play.

If there are already two requests for games in the queue, the main
thread will have to wait until there are less than two before adding
another. If there are less than two requests for games in the queue,
obviously the `gameStarter` thread will have to wait before it can 
create a game.

The `BoundedChan` type serves this purpose. It provide a *bounded
channel*, or concurrent queue that can contain only a given number of
elements; threads that want to take from the queue are blocked if it
is empty, and threads that want to add to a full queue are blocked
until there is room for another element.

The `main` function sets up some logging using the `hslogger` package,
creates the bounded channel and forks a new thread to watch the
channel, running the `gameStarter` thread.  It then runs the websocket
application, called `enqueue`, passing in a reference to the bounded 
channel.

```haskell
-- | Entry point for the server. It begins by setting up the logger.
--   It then creates the Chan which will hold incoming clients, starts the thread
--   that will watch the Chan and start games, and continues to listen for
--   connections.
main :: IO ()
main = do
  args <- getArgs
  -- Parse options, getting a list of option actions
  let (actions, _, _) = getOpt RequireOrder options args
  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optConf = path } = opts
  conf <- parseConf path defaultConf
  let pr = read (T.unpack $ log_priority conf) :: Priority
  updateGlobalLogger "Scrabble" (setLevel pr)
  h <- fileHandler (T.unpack $ log_file conf) pr >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "Scrabble" (addHandler h)
  infoM "Scrabble" ("Starting server with conf "<>show conf)
  state <- newBoundedChan 2
  _ <- forkIO (gameStarter state)
  WS.runServer (T.unpack $ hostname conf) (port conf) $ enqueue state
```

The `enqueue` function loops forever accepting incoming
connections. When a connection comes in it reads from it
and tries to convert the incoming data to a `MsgJoin`. If this
can't be done the request is rejected.

If this is a request for an AI game, it begins straight away in a new
thread using the `WebSocket` function `withPingThread`. As well as
running the thread this keeps the connection alive by sending a "ping"
(an empty message) to the client every now and then. The `enqueue`
server then loops for the next connection. 

If the request is for an interactive game then `enqueue` adds the
request to the queue and keeps looping. When we add a request to the
queue, there's no telling how long we may have to wait for another
client to request an interactive game, so again we fork a pinging
thread to keep that connection alive while it waits.

```haskell
-- | Makes incoming connections into Clients and adds them to the queue.
enqueue :: BoundedChan Client -> WS.ServerApp
enqueue state pending = do
    conn <- WS.acceptRequest pending
    msg  <- WS.receiveData conn
    case decode msg of
      Nothing -> WS.sendTextData conn ("Bad input: " <> msg)
      Just (MsgJoin (name,ai)) -> do
        infoM "Scrabble" ("[Client] " <> T.unpack name <> " [AI] " <> show ai)
        -- If the client wants an AI game, start right away. Otherwise, put them in the queue.
        if ai
          then WS.withPingThread conn 30 (return ()) (aiGame (name,conn))
          else do
          writeChan state (name,conn) 
          -- keep the connection alive by having a thread that listens to it
          WS.withPingThread conn 30 (return ()) loop
            where loop = threadDelay (10000*5) >> loop
      Just _ -> WS.sendTextData conn ("Not expecting: " <> msg)
```

Now we come to the `gameStarter` action. This runs in its own thread,
watching the channel until it can take two things from it. When that
happens it makes sure the names are distinct by altering the second
one if necessary, then creates a game and passes it to the `playGame`
function in a new thread.

```haskell
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
          _ <- forkIO $ playGame (newGame (n1',c1) (n2',c2) ig)
          loop
```

From this point onwards, the functions we write (`playGame`,
`takeTurn`, `takeTurnManual` and `takeTurnAI`) are very
similar to the versions from the CLI client. The biggest difference is
of course that non-AI clients interact with the server using JSON. In
the `takeTurnManual` function a message is read from the client
connection and decoded. There follows a case statement that pattern
matches the message. If the message can't be decoded or isn't one of
the messages the server is expecting, it loops back to
`takeTurn`. Otherwise, the message is a request for hints or one of
the ways of playing a move.

```haskell
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
```

The functions that send feedback to clients are defined in the module
`ScrabbleWeb.Announce`. Note that at this level `MoveAck` is sent to
both clients without checking whether this an AI game. The `send`
function in `ScrabbleWeb.Announce` will check whether a client is an
AI player before trying to send any message.

## Writing the client


The client is written as a small, free-standing (i.e. one that doesn't
require a webserver) web application, with most of the effort going in
to the JavaScript. I'm not going to go into any detail about the code,
as JavaScript techniques aren't the subject of this book.  Suffice to
say, the event handler that receives incoming messages from the server
has a `switch` statement (the equivalent of Haskell's `case`
statement) that pattern matches on messages in a very similar way to
the one in `takeTurnManual` above, except that it is expecting a
different set of messages.

<img src="/chapters/images/webgame.png" alt="Playing Scrabble on the web" width="500px" />

At the top of the file `web/client/html/main.js` is a multiline comment
that lists the data dictionary and gives the JSON equivalent of all
`Msg` values.

Clients can be started by opening `web/client/html/index.html` in your
browser. On the first screen you need to enter a name and you can
click on the checkbox to start a game against the computer. If you
leave that unchecked the server will wait for a second connection,
which you can supply by opening the index page in a second tab.

In the final chapter we will add a configuration system to
the server that allows you to change the hostname, port and other
server details without needing to recompile it.

## Exercises

[Contents](../README.md) | [Chapter Eight](Chapter8.md)
