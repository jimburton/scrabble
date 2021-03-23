# Chapter Five: The CLI client

The first client is a terminal-based CLI (command line interface). It's probably
the case that nobody would want to play a two-player game of Scrabble this way, because
you can see each other tiles. But it does work well enough for a one-player game and
above all else it's quite a straightforward way to understand the general problem
of writing clients that use the library and provide a user interface.

The basic idea is that we'll write code that takes input from users in a loop and that
keeps playing moves in the game until it registers the fact that the game has ended.

This code will live in a separate area to the library. We wtore it under the directory
`cli/` and add an `executable` stanza to the config file. The `cli` directory contains
these files:

```
cli/
├── Main.hs
└── ScrabbleCLI
    ├── Blanks.hs
    ├── Game.hs
    └── Out.hs
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
  p2Str <- T.getLine
  startGame p1Str p2Str

doAIGame :: IO ()
doAIGame = do
  T.putStrLn "Enter name of player"
  pStr <- T.getLine
  startGameAI pStr
```
The main effort goes into `ScrabbleCLI.Game`. The other modules, `ScrabbleCLI.Blanks` and
`ScrabbleCLI.Out`, handle blank tiles and output to the user respectively.

## Interacting with users to play the game

`ScrabbleCLI.Game` contains the code that interacts with users: taking input that it interprets
as moves, passing that to the library and providing users with the response. The first thing we
need is to be able to start a game. This is dealt with in two functions, each of which creates
a new `Game` object then calls the `playGame` function.

```haskell
startGame :: Text -> Text -> IO ()
startGame p1Name p2Name = do
  theGen <- getStdGen
  d      <- englishDictionary
  _ <- playGame (newGame p1Name p2Name theGen d)
  return ()

startGameAI :: Text -> IO ()
startGameAI p1Name = do
  theGen <- getStdGen
  d      <- englishDictionary
  _ <- playGame (newGame1P p1Name theGen d)
  return ()
```
The `playGame` function just prints the details of the two players then passes the game
to the `takeTurn` function, which is the top level of the loop that actually plays the game.
It prints the current state of the board then checks whether the game is over. If so, it
prints the result. If not, it checks whose turn it it. If the current player is a human
player, it calls the function that reads a move from the terminal. Otherwise, it calls the
function that plays an AI move.

```haskell
takeTurn :: Game -- ^ The game
         -> Maybe Text -- ^ Previous score as text
         -> IO Game
takeTurn g msc = runInputT defaultSettings loop
 where
   loop :: InputT IO Game
   loop  = do
     liftIO $ printBoard True (board g) msc
     if gameOver g
       then liftIO $ doGameOver g
       else if isAI (getPlayer g)
            then liftIO $ takeTurnAI g
            else liftIO $ takeTurnManual g

```

  ```
  $ git checkout v-01
  $ cabal repl
  $ startGame "Bob" "Alice"
  **********************************************
  ```
  
  *The game starts by showing both player's names, their score and the contents
  of their rack, with the score displayed beneath each letter.*
  
  
  ```
  Bob (0)
  A, T, F, H, E, D, A
  1, 1, 4, 4, 1, 2, 1
  **********************************************


  **********************************************
  Alice (0)
  W, O, D, E, E, Q, V
  4, 1, 2, 1, 1, 10, 4
  **********************************************
  ```
  
  *Then, for each turn it displays the current board, the current player's rack
  and prompts them for a move.*
  
  
  ``` 

    | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
  ------------------------------------------------
   0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   5|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   7|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   8|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   9|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  10|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  11|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  ------------------------------------------------


  **********************************************
  Bob (0)
  A, T, F, H, E, D, A
  1, 1, 4, 4, 1, 2, 1
  **********************************************
  Enter WORD ROW COL DIR[H/V]:
  
  ```
  *Bob enters a move. The resulting board and the score for the move 
  is displayed.*
  
  ```
  fated 7 7 v
    | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
  ------------------------------------------------
   0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   5|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   7|  |  |  |  |  |  |  | F|  |  |  |  |  |  |  |
   8|  |  |  |  |  |  |  | A|  |  |  |  |  |  |  |
   9|  |  |  |  |  |  |  | T|  |  |  |  |  |  |  |
  10|  |  |  |  |  |  |  | E|  |  |  |  |  |  |  |
  11|  |  |  |  |  |  |  | D|  |  |  |  |  |  |  |
  12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  ------------------------------------------------
  22

  ```
  
  *Alice is prompted for a move, enters it, and so on.*
  
  
  ``` 
  **********************************************
  Alice (0)
  W, O, D, E, E, Q, V
  4, 1, 2, 1, 1, 10, 4
  **********************************************
  Enter WORD ROW COL DIR[H/V]:
  wode 11 5 h
    | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
  ------------------------------------------------
   0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   5|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
   7|  |  |  |  |  |  |  | F|  |  |  |  |  |  |  |
   8|  |  |  |  |  |  |  | A|  |  |  |  |  |  |  |
   9|  |  |  |  |  |  |  | T|  |  |  |  |  |  |  |
  10|  |  |  |  |  |  |  | E|  |  |  |  |  |  |  |
  11|  |  |  |  |  | W| O| D| E|  |  |  |  |  |  |
  12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
  ------------------------------------------------

  8

  ```
  The code lives in the `src` directory.
  
  **Things to look out for in the code:**
  
  + The `Main` module contains the `startGame` function and the `playGame` and `takeTurn`
    functions that play the game.
	The use of `getLine` in `takeTurn` means that if you try to use the backspace
	key when entering a move it will show up as garbage (we'll fix this in the next version). 
	Type carefully or you will have to quit the game with `Ctrl-C` and start again.
  + To toggle the display of bonus squares on the board (double and triple letter squares, double
    and triple word squares), go to the `takeTurn` function and  change the boolean in the call to 
	`printBoard` from `False` to `True`.

  + The datatypes live in the module `Scrabble.Types`.

  + The main code for playing the game is in `Scrabble.Game`. Start by
    studying the functions `playGame` and `takeTurn` in there. Look
    for the code that fills a `Bag` with letters then takes letters
    from the bag to fill the `Rack` of each `Player`.
  + Moves are validated in the `playGame` function by other functions whose
    names begin `validate...`q. They each return
    `Either String a` where the String is an error message and `a` may
    be any type.  After each validation function is called we need to check whether the result
	is an error message (`Left e`) or a "good" result (`Right x`). The following things are 
	validated:
	+ Does the first move touch the centre square on the board?
	+ Is this move made up of letters that are either in the player's rack or on the board?
	+ Were some new tiles played onto the board in this move?
	+ Is the new word in the dictionary?
	+ Are all of the additional words generated by playing this word in the dictionary? To understand
	  the idea of additional words being generated, look at the following board (with bonus squares
	  shown):
	  
	  ```
	    | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
      ------------------------------------------------
       0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
       1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
       2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
       3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
       4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
       5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
       6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
       7|W3|  |  |L2|  |  |  | F|  |  |  |L2|  |  |W3|
       8|  |  |L2|  |  |  |L2| O|L2|  |  |  |L2|  |  |
       9|  |L3|  |  |  | N| O| U| S|L3|  |  |  |L3|  |
      10|  |  |  |  |W2|  |  | L|  |  |W2|  |  |  |  |
      11|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
      12|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
      13|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
      14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
      ------------------------------------------------
      
      6
      
      **********************************************
      Bob (14)
      Z, D, R, T, Blank, E, I
      10, 2, 1, 1, 0, 1, 1
      **********************************************
      Enter WORD ROW COL DIR[H/V]:
      toe 8 6 v
	  ```
	  Bob plays the word TOE vertically, starting at position (8,6). 'T' is worth 1 point but is
	  played on a double-letter bonus square so it contributes 2 to the score for the word. 'O'
	  and 'E' are each worth one point so the 
	  score for TOE is 4. Placing this word generates two additional
	  words, TO ((8,6), horizontal) and EL ((10,6) horizontal). The bonus underneath 'T' is counted 
	  again in the score for TO. The score for the additional words is added to the 
	  score for TOE, but any bonus squares under the existing tiles do not count (there aren't any
	  in this case). 
	  
	  The final score for the move is 9.
	  
	  ```
        | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
      ------------------------------------------------
       0|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
       1|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
       2|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
       3|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
       4|  |  |  |  |W2|  |  |  |  |  |W2|  |  |  |  |
       5|  |L3|  |  |  |L3|  |  |  |L3|  |  |  |L3|  |
       6|  |  |L2|  |  |  |L2|  |L2|  |  |  |L2|  |  |
       7|W3|  |  |L2|  |  |  | F|  |  |  |L2|  |  |W3|
       8|  |  |L2|  |  |  | T| O|L2|  |  |  |L2|  |  |
       9|  |L3|  |  |  | N| O| U| S|L3|  |  |  |L3|  |
      10|  |  |  |  |W2|  | E| L|  |  |W2|  |  |  |  |
      11|L2|  |  |W2|  |  |  |L2|  |  |  |W2|  |  |L2|
      12|  |  |W2|  |  |  |L2|  |L2|  |  |  |W2|  |  |
      13|  |W2|  |  |  |L3|  |  |  |L3|  |  |  |W2|  |
      14|W3|  |  |L2|  |  |  |W3|  |  |  |L2|  |  |W3|
      ------------------------------------------------
      
      9
	  ```

  + To see how the score is calculated, study the functions
    `scoreWord` and `scoreWords` in `Scrabble.Board`. Every tile in a
    new word adds its face value multiplied by the letter-bonus of the
    square it is placed on, if any. Then the score for the whole word
    is multiplied by any word-bonus that a new tile has been placed
    on. A `Map` called `bonusMap` is maintained of the positions of
    bonus squares on the board. Bonus squares can double or triple the
    value of the tile played on them (shown in the ASCII board as `L2`
    and `L3`) and double or triple the value of entire words (`2W` and
    `3W`). However, bonuses are only applied once, when a new tile has been
    placed on them. If all seven tiles are played in a move there is a fifty
	point bonus.
	
  + It may be convenient to switch off the dictionary checking in
	development or while playing with the code so that you don't have to come up with a valid word
	every time you want to enter a move. You can switch the dictionary
	checking off or on by changing the call to the function
	`validateGameRulesAndDict` in `Scrabble.playGame` to
	`validateGameRules`.
  + Blank tiles can't be played for now.
  + There is no way to check whether the game has ended or who won.
  
## Miscellaneous improvements to make the game more playable

+ **`v-0.4`**:
  + "Commands" are added to the CLI. These are strings entered by the user that begin
    with a colon. This is supported by the `cmd` function in `cli/Main.hs`.
    + `:HINT` command. The current player is able to ask for hints based on the letters in their rack. 
    + `:SWAP` command. This allows the current player to sacrifice a move by swapping some tiles.
	+ `:PASS` command. This allows the current player to sacrifice a move altogether without
	  swapping tiles (needed at the end of the game if the bag is empty and they still have
	  tiles but can't go).
  + We can now detect when the game is over and announce the winner,
    if there is one. A boolean called `gameOver` is added to the
    `Game` object, initially set to false. It is the responsibility of
    clients to check it.  A game ends in one of two ways:
	
	+ each player passes their move, or
	+ the bag and both racks are empty.

    To detect that there have been two passes in a row, a boolean
	called `lastMovePass` is added to the `Game` object, initially set
	to false. When a player wants to pass a move, if `lastMovePass` is
	true then `gameOver` is set to true. Otherwise, `lastMovePass` is
	set to true. When a player takes a move by playing or swapping,
	`lastMovePass` is set to false.
	
	The code to detect that there are no tiles left is in `Scrabble.Game.Game.toggleTurn`.
	
  + Blanks can be played. If the player has one or more blanks in their rack they can enter a word 
    containing underscores. They are then queried for the letter they want the blank to stand for.
	A `WordPut` is created in which each "unblanked" letter is given a score of zero, so that the
	move will eventually generate the right score. The unblanked 
	word is passed from the CLI to the `move` function in the library along with a list 
	of the indices in the `WordPut` that were originally blanks (this is needed so that the code that
	validates the move doesn't complain about the player not having those letters in their rack).


[Contents](../README.md) | [Chapter Six](Chapter6.md)
