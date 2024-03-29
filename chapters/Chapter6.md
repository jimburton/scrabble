# Chapter Six: The CLI client

[Contents](../README.md)

The first client is a terminal-based CLI (command line
interface). It's probably true that nobody would want to play a
two-player game of Scrabble this way -- you can see each other's
tiles. But it does work well enough for a one-player game, and above
all else it's a straightforward way to understand the general problem
of writing clients that use the library and provide a user
interface. All the CLI client needs to do is to read keystrokes from
the user, interact with the library and print the board to the
terminal.

This code will live in a separate area to the library. We store it
under the directory `cli/` and add an `executable` stanza to the
config file. The `cli` directory contains these files:

```
cli/
├── Main.hs
└── ScrabbleCLI
    ├── Blanks.hs
    ├── Game.hs
    └── Out.hs
```

The new stanza in the config file is shown below. It needs to list all
of its own modules plus all of the library modules that it imports, as
well as all the source folders in which these modules can be found,
and any libraries it imports -- everything that `cabal` needs to compile
it.

```
executable scrabble
  main-is:             Main.hs
  other-modules:       ScrabbleCLI.Game
                     , ScrabbleCLI.Out
                     , ScrabbleCLI.Blanks
                     , Scrabble
                     , Scrabble.Types
                     , Scrabble.Evaluator
                     , Scrabble.Game.Game
                     , Scrabble.Game.AI
                     , Scrabble.Game.Internal
                     , Scrabble.Game.Validation
                     , Scrabble.Lang.Dict
                     , Scrabble.Lang.Letter
                     , Scrabble.Lang.Word
                     , Scrabble.Lang.Search
                     , Scrabble.Board.Bag
                     , Scrabble.Board.Board
                     , Scrabble.Board.Bonus
                     , Scrabble.Board.Pretty
                     , Scrabble.Board.Validation
                     , Scrabble.Board.Internal
  ghc-options: -Wall -fno-warn-orphans
  build-depends:       base >=4.12 && <4.13
                     , random
                     , array
                     , text
                     , text-trie
                     , split
                     , containers
                     , haskeline
  hs-source-dirs:      cli
                     , src
  default-extensions: OverloadedStrings
  default-language:    Haskell2010

```
The `Main` module provides a way for users to start a one or two-player game then 
passes control to functions in `ScrabbleCLI.Game`. Here is the `Main` module in
full:

```haskell
import Data.Text (toUpper)
import qualified Data.Text.IO as T
import ScrabbleCLI.Game (startGame, startGameAI)

-- ========= Entry point for a CLI game of Scrabble =========== --

main :: IO ()
main = do
  T.putStrLn "Enter 1P or 2P"
  str <- fmap toUpper T.getLine
  case str of
    "1P" -> doAIGame
    "2P" -> doManualGame
    _    -> main

doManualGame :: IO ()
doManualGame = do
  T.putStrLn "Enter name of Player 1"
  p1Str <- T.getLine
  T.putStrLn "Enter name of Player 2"
  T.getLine >>= startGame p1Str

doAIGame :: IO ()
doAIGame = do
  T.putStrLn "Enter name of player"
  T.getLine >>= startGameAI
```
Most of the effort goes into `ScrabbleCLI.Game`. The other modules, `ScrabbleCLI.Blanks` and
`ScrabbleCLI.Out`, handle blank tiles and output to the user respectively.

## Interacting with users to play the game

`ScrabbleCLI.Game` contains the code that interacts with users: taking
input that it tries to interpret as moves, passing that to the library
and providing users with the response. 

The first thing we need is to be able to start a game. This is dealt
with in two functions, each of which creates a new `Game` object then
calls the `playGame` function. (The result of the `playGame` action is
bound to the wildcard identifier `_` just to suppress the warning about
its result, which is of type `IO Game`, being discarded.)


```haskell
import Data.Functor (($>))

startGame :: Text -> Text -> IO ()
startGame p1Name p2Name = do
  theGen <- getStdGen
  d      <- englishDictionary
  playGame (newGame p1Name p2Name theGen d) $> ()

startGameAI :: Text -> IO ()
startGameAI p1Name = do
  theGen <- getStdGen
  d      <- englishDictionary
  playGame (newGame1P p1Name theGen d) $> ()
```
The `playGame` action prints the details of the two players then passes the game
to the `takeTurn` function. This is the top level of the loop that actually plays 
the game. Note the use of the `($>)` operator -- this performs the action from its
first argument then lifts its second argument into the functor. So `f $> x` is the 
same as `f >> pure x`.

To make the process of entering text nicer we use the `haskeline`
library. That means we can use the backspace and arrow keys when
entering a move -- otherwise we would have to type everything
perfectly the first try. `haskeline` reads input from users into its
own monadic type, `InputT`. Any time we want to run an `IO` action
inside an `InputT` action we need to "lift" the `IO` action using 
`liftIO` which has the type `MonadIO m => IO a -> m a`. `InputT` is
an instance of `MonadIO`, which is the class of types in which `IO`
actions can be embedded.

The `takeTurn` function prints the current state of the board then
checks whether the game is over. If so, it prints the result. If not,
it checks whether the player whose turn it is is an AI player. If the
current player is a human it calls the function that reads a move from
input. Otherwise, it calls the function that plays a move
automatically.

```haskell
-- | Take a turn.
takeTurn :: Game       -- ^ The game
         -> Maybe Text -- ^ Previous score as text
         -> IO Game
takeTurn g msc = runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     liftIO $ printBoard False (g ^. board) msc
     if g ^. gameOver
       then liftIO $ doGameOver g
       else if g ^. (getPlayer g . isAI)
            then liftIO $ takeTurnAI g
            else liftIO $ takeTurnManual g
```

The second argument to `takeTurn` is a `Maybe Text` that allows us to
display an optional message to the user. Within the function the
`gameOver` field of the game is checked. If it is true, we pass the game
to the `doGameOver` function. Otherwise, either an AI player or a human
player takes a turn.

```haskell
-- | Handle the situation when the game ends.
doGameOver :: Game -> IO Game
doGameOver g = do
  let p1     = g ^. player1
      p2     = g ^. player2
      draw   = p1 ^. score == p2 ^. score
      winner = if p1 ^. score > p2 ^. score
               then p1 else p2
  T.putStrLn "Game over!"
  T.putStrLn $ p1 ^. name <> ": " <> T.pack (show (p1 ^. score))
  T.putStrLn $ p2 ^. name <> ": " <> T.pack (show (p2 ^. score))
  if draw
    then T.putStrLn "It's a draw!" $> g
    else T.putStrLn ("Congratulations " <> winner ^. name) $> g
```

## Taking a turn as the AI player

Let's look first at the simpler case of `takeTurnAI`. It passes the game to
`moveAI` and pattern matches on the result. If the result was `Ev (Left e)`
is reports the error and returns the unchanged game to the loop in `takeTurn`.
If the move succeeded it passes the updated game and a `Maybe Text` containing
the score to `takeTurn.

```haskell
-- | Allow the computer to take a turn.
takeTurnAI :: Game -> IO Game
takeTurnAI g = case moveAI g of
  Ev (Right (g',mr)) -> takeTurn g' (Just (T.pack $ show mr))
  Ev (Left e)       -> T.putStrLn e $> g
```

That's it. The library takes care of everything else.

## Taking a turn as the human player

When a human player takes a turn there
are more things to consider. First, we need to read some input from the user
in an `InputT` action. If they enter an empty line, we keep looping. Otherwise,
we try to parse the input. 

```haskell
-- | Take a turn manually.
takeTurnManual :: Game -- ^ The game
               -> IO Game
takeTurnManual g = runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     mLn <- getInputLine (T.unpack $ showTurn g)
     case mLn of
       Nothing -> loop
       Just wdStr -> do
         let wds = words wdStr
         if null wds || (length wds /= 4 && head (head wds) /= ':')
           then loop
           else do
           if head (head wds) == ':'
             then liftIO $ do
             let c = head wds
             (g',ms) <- cmd (T.map toUpper (T.pack c), T.pack <$> mLn, g)
             takeTurn g' ms
             else do
             (wd,is) <- liftIO $ replaceBlanks (head wds)
             let [rowStr,colStr,dirStr] = tail $ words wdStr
                 row = read rowStr :: Int
                 col = read colStr :: Int
                 dir = if map toUpper dirStr == "H" then HZ else VT
                 wp  = makeWordPut (T.pack wd) (row,col) dir is
             case move valGameRules g wp is of
               Ev (Left e) -> do liftIO $ T.putStrLn e
                                 liftIO $ takeTurn g $ Just (T.pack wd  <> ": NO SCORE")
               Ev (Right (g',mr)) -> liftIO $ takeTurn g' (Just (T.pack $ show mr))
```
If the input begins with a colon (':'), it is treated as a
"command". Commands are builtin functions that allow the user to:

+ pass the move by entering `:PASS`
+ swap some tiles by entering `:SWAP <TILES>`, e.g. `:SWAP ABC`
+ print a help message by entering `:HELP`
+ get a list of all words that can be made with her tiles by entering `:HINT`.

This is handled by a new datatype, `Cmd`, the `getCmd` function that parse the input, and 
functions that handle swapping, passing, help and hints. After running the command, the 
`takeTurn` function is called again.

```haskell
-- | Datatype for commands entered by the user.
data Cmd = Swap | Pass | Hint | Help | Unknown deriving (Show, Eq)

-- | Read a command.
getCmd :: Text -> Cmd
getCmd ":SWAP" = Swap
getCmd ":PASS" = Pass
getCmd ":HINT" = Hint
getCmd ":HELP" = Help
getCmd _       = Unknown

-- | Deal with commands entered by a player
cmd :: (Text, Maybe Text, Game) -> IO (Game, Maybe Text)
cmd (s, mLn, g) = case getCmd s of
                    Swap    -> doSwap (g, mLn)
                    Pass    -> doPass (g, mLn) 
                    Hint    -> hints g $> (g, mLn)
                    Help    -> help $> (g, mLn)
                    Unknown -> pure (g, mLn)
```

The `doSwap` function reads letters from the user until they enter an empty line
and tries to swap these tiles. It needs to unwrap the `Evaluator` result.

```haskell
-- | Take a move by swapping some tiles.
doSwap :: (Game, Maybe Text) -> IO (Game, Maybe Text)
doSwap (g, mLn) = do
  putStrLn "Enter tiles to swap and type return when done:"
  ln <- getLine
  case swap (fromJust $ stringToWord (map toUpper ln)) g of
    Ev (Right g') -> pure (g',mLn)
    Ev (Left e)   -> T.putStrLn e $> (g,mLn)
```
The `doPass` function is very similar.

```haskell
-- | Take a move by passing.
doPass :: (Game, Maybe Text) -> IO (Game, Maybe Text)
doPass (g, mLn) = case pass g of
  Ev (Right g') -> pure (g', Just "Passed move")
  Ev (Left e)   -> T.putStrLn e $> (g,mLn)
```
The `hints` function calls `findPrefixes` to find all of the words in the 
dictionary that can be made with the current player's rack.

```haskell
-- | Print some word suggestions based on the current player's rack.
hints :: Game -> IO ()
hints g = do
  let w = rack (getPlayer g) 
  T.putStrLn "HINTS:"
  mapM_ print $ findPrefixes g w

```
If the input didn't start with a colon, we expect it to be of the form 

```
WORD ROW COL DIR
```

where `WORD` is a word made from the player's rack and (after the
first move) tiles on the board, `ROW` and `COL` are numbers
representing a row and a column respectively, and `DIR` is either `H`
(horizontal) or `V` (vertical). Let's repeat this part of the
`takeTurnManual` to look at how this input is parsed.

```haskell
(wd,is) <- liftIO $ replaceBlanks (head wds)
let [rowStr,colStr,dirStr] = tail $ words wdStr
    row = read rowStr :: Int
    col = read colStr :: Int
    dir = if map toUpper dirStr == "H" then HZ else VT
    wp  = makeWordPut (T.pack wd) (row,col) dir is
case move valGameRules g wp is of
  Ev (Left e) -> do liftIO $ T.putStrLn e
                    liftIO $ takeTurn g $ Just (T.pack wd  <> ": NO SCORE")
  Ev (Right (g',mr)) -> liftIO $ takeTurn g' (Just (T.pack $ show mr))
```

First we replace any blanks that were in the `WORD` part of the
input. This is done by interrogating the user and producing a list of
pairs of replacements and their indices. This code is in
`ScrabbleCLI.Blanks` and we won't go through it here. Then we select the
other parts of the input with the `words` function, the `makeWordPut`
function is used to create a `WordPut` from what we have, and the game
and the `WordPut` are passed to the `move` function. Finally, the
`Evaluator` result is pattern matched. As with `takeTurnAI`, we either
print an error message and send the unchanged game to the `takeTurn` loop,
or send the new, updated game to `takeTurn`.

## Running the game

We no longer need to call functions in the REPL. Now we can actually run the game.
Here is the first couple of moves of an AI game in which the human player asks
for hints and at one point plays a blank tile.

```
$ cabal run scrabble
Enter 1P or 2P
1P
Enter name of player
Bob

**********************************************
Bob (0)
O, I, C, E, T, L, R
1, 1, 3, 1, 1, 1, 1
**********************************************


**********************************************
Haskell (0)
I, P, B, J, U, F, X
1, 3, 3, 8, 1, 4, 8
**********************************************

  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
 1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
 2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
 3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
 4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
 5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
 6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 7|W3|  |  |L2|  |  |  |W2|  |  |  |L2|  |  |W3|
 8|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 9|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
10|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
11|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
12|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
13|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
------------------------------------------------


**********************************************
Bob (0)
O, I, C, E, T, L, R
1, 1, 3, 1, 1, 1, 1
**********************************************
Enter WORD ROW COL DIR[H/V]:
:hint
HINTS:
[E,R]
[R,E]
<long list of hints elided>
[R,E,C,O,I,L]
[T,E,R,C,I,O]
[E,R,O,T,I,C]
[C,I,T,O,L,E]
[C,O,R,T,I,L,E]
  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
 1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
 2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
 3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
 4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
 5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
 6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 7|W3|  |  |L2|  |  |  |W2|  |  |  |L2|  |  |W3|
 8|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 9|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
10|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
11|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
12|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
13|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
------------------------------------------------

**********************************************
Bob (0)
O, I, C, E, T, L, R
1, 1, 3, 1, 1, 1, 1
**********************************************
Enter WORD ROW COL DIR[H/V]:
cortile 7 7 v
  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
 1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
 2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
 3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
 4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
 5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
 6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 7|W3|  |  |L2|  |  |  | C|  |  |  |L2|  |  |W3|
 8|  |  |L2|  |  |  |L2| O|L2|  |  |  |L2|  |  |
 9|  |L3|  |  |  |L3|  | R|  |L3|  |  |  |L3|  |
10|  |  |  |  |W2|  |  | T|  |  |W2|  |  |  |  |
11|L2|  |  |W2|  |  |  | I|  |  |  |W2|  |  |L2|
12|  |  |W2|  |  |  |L2| L|L2|  |  |  |W2|  |  |
13|  |W2|  |  |  |L3|  | E|  |L3|  |  |  |W2|  |
14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
------------------------------------------------

CORTILE 70

| 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
 1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
 2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
 3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
 4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
 5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
 6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 7|W3|  |  | P| U| B| I| C|  |  |  |L2|  |  |W3|
 8|  |  |L2|  |  |  |L2| O|L2|  |  |  |L2|  |  |
 9|  |L3|  |  |  |L3|  | R|  |L3|  |  |  |L3|  |
10|  |  |  |  |W2|  |  | T|  |  |W2|  |  |  |  |
11|L2|  |  |W2|  |  |  | I|  |  |  |W2|  |  |L2|
12|  |  |W2|  |  |  |L2| L|L2|  |  |  |W2|  |  |
13|  |W2|  |  |  |L3|  | E|  |L3|  |  |  |W2|  |
14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
------------------------------------------------

PUBIC 13

**********************************************
Bob (70)
Y, N, O, E, R, G, _
4, 1, 1, 1, 1, 2, 0
**********************************************
Enter WORD ROW COL DIR[H/V]:
green_ 13 5 h
Enter a letter for the blank:s

| 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
 1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
 2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
 3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
 4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
 5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
 6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 7|W3|  |  | P| U| B| I| C|  |  |  |L2|  |  |W3|
 8|  |  |L2|  |  |  |L2| O|L2|  |  |  |L2|  |  |
 9|  |L3|  |  |  |L3|  | R|  |L3|  |  |  |L3|  |
10|  |  |  |  |W2|  |  | T|  |  |W2|  |  |  |  |
11|L2|  |  |W2|  |  |  | I|  |  |  |W2|  |  |L2|
12|  |  |W2|  |  |  |L2| L|L2|  |  |  |W2|  |  |
13|  |W2|  |  |  | G| R| E| E| N| S|  |  |W2|  |
14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
------------------------------------------------

GREENS 12
  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
 1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
 2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
 3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
 4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
 5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
 6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
 7|W3|  |  | P| U| B| I| C|  |  |  |L2|  |  |W3|
 8|  |  |L2| E|  |  |L2| O|L2|  |  |  |L2|  |  |
 9|  |L3|  | R|  |L3|  | R|  |L3|  |  |  |L3|  |
10|  |  |  | T|W2|  |  | T|  |  |W2|  |  |  |  |
11|L2|  |  |W2|  |  |  | I|  |  |  |W2|  |  |L2|
12|  |  |W2|  |  |  |L2| L|L2|  |  |  |W2|  |  |
13|  |W2|  |  |  | G| R| E| E| N| S|  |  |W2|  |
14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
------------------------------------------------

PERT 6

**********************************************
Bob (82)
N, V, T, P, O, Y, O
1, 4, 1, 3, 1, 4, 1
**********************************************
Enter WORD ROW COL DIR[H/V]:
```
## Wrapping up

We have seen that writing a client for the Scrabble library meant
writing `IO`-bound code that interacts with the user and calling the
functions in the library that know how to take a move. We could easily
extend this approach to clients with different (mor realistic) interfaces 
written for, say, mobile devices or  the desktop.

In the next chapter, however, we make a much more general interface
for networked clients which is based on a web service, thus decoupling
the library from clients altogether. From then on, clients can be written
in any language, and we'll write one in Javascript.

## Tests

TODO

## Exercises

1. The code that parses input from the user in `takeTurnManual` is too fragile. 
   If the user does not enter four strings corresponding to a word, column number,
   row number and direction, the pattern matching will fail and the game will crash. 
   Refactor the function to check the following conditions:
   
   1. `words wdStr` returns a list of four strings,
   2. the first string (the word) contains only letters,
   3. the second and third contain only digits,
   4. the fourth (the direction) is either "H" or "V".

   If any of these conditions is not true, print a helpful message and call 
   `loop` again.


[Contents](../README.md) | [Chapter Seven](Chapter7.md)
