
# Scrabble

An multiplayer implementation of Scrabble for teaching functional programming at the 
University of Brighton.

The Scrabble library is adapted from https://github.com/joshcough/Scrabble, while the UI 
borrows code from https://github.com/danielweck/scrabble-html-ui.

If you don't know the rules of the game you can read up on them [here](https://www.theukrules.co.uk/rules/children/games/scrabble.html).

The emphasis is intended to be not so much on the logic of playing the
game as on refining an initial solution to a clean, functional software design
that can easily be extended. The evolution of the code is demonstrated
in branches that the student should study in this order:

## The first version

+ **`v-0.1`**: This branch contains the initial effort. It includes
  the basic data types (`Letter` , `Word`, `Board`, `Bag`, `WordPut`,
  which is a word along with its position on the board, `Player`,
  `Game` and so on). There are functions for starting a game, taking turns in which
  players put words onto the board, and showing the board and the
  score. The dictionary is held as a `Set` of `Text` values. There is
  no UI but you can start a game and take a few moves in the REPL like
  this:
  
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
	  played on a double-letter bonus square so it contributes 2 to the score for the word. So the 
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
    placed on them.
	
  + It may be convenient to switch off the dictionary checking in
	development or while playing with the code so that you don't have to come up with a valid word
	every time you want to enter a move. You can switch the dictionary
	checking off or on by changing the call to the function
	`validateGameRulesAndDict` in `Scrabble.playGame` to
	`validateGameRules`.
  + Blank tiles can't be played for now.
  + There is no way to check whether the game has ended or who won.
  
## Organising the code

+ **`v-0.2`**:
  + The code is split into a *library* (in the directory `src`),
    that contains all the code for managing a game of Scrabble, and an
    *executable* (in the directory `cli`), which contains the
    command-line interface (CLI).  This will make it easy to reuse the core game code in
    multiple UIs, such as a web-based one or a mobile app that we might build later. 
	Study the `cabal` config file to see how this division of code is organised. 
	
	Code is in the library is reorganised so that related code is kept
    together and easy to find.  Even though quite a lot more code is
    added, each module is kept down to a manageable size. Each module exports only those
	datatypes, constructors and functions the outside world needs, and imports only what it 
	needs. Files in the library:
	
	```
	src
    ├── Scrabble
    │   ├── Board
    │   │   ├── Bag.hs 
    │   │   ├── Board.hs
    │   │   ├── Bonus.hs
    │   │   └── Validation.hs
    │   ├── Evaluator.hs
    │   ├── Game
    │   │   ├── Game.hs
    │   │   └── Validation.hs
    │   ├── Lang
    │   │   ├── Dict.hs
    │   │   ├── Letter.hs
    │   │   ├── Search.hs
    │   │   └── Word.hs
    │   ├── Show.hs
    │   └── Types.hs
    └── Scrabble.hs

	```
  + To play the game you now need to enter `cabal repl scrabble`.
  + Use of the `haskeline` library to enable using backspace and arrow keys in the CLI.
    Note that this changes to the `takeTurn` function in `cli/Main.hs`.

## Optimising the data structures and making good use of functional design patterns.

+ **`v-0.3`**:
  + The dictionary is now stored in a [trie](https://en.wikipedia.org/wiki/Trie), which is a very
    efficient data structure for storing strings sorted by their prefixes. This speeds up the
    process of checking whether words exist.
  + The `String` datatype is used a lot in earlier versions. In this version this is almost all 
    replaced with `Data.Text`, which is much more efficient. 
  + The biggest change in the library is the introduction of monadic `Either` code. A new type
    called `Evaluator` is added (see `Scrabble.Types` and `Scrabble.Evaluator`). This type wraps
	up an `Either String a` type, where the `String` is an error message and the `a` value is
	whatever is being evaluated (e.g. an updated version of the game, or just `()` in cases where
	moves are validated). By making this type into a monad we are able to replace all of
	the nested case statements in `takeTurn` with monadic-style code. For example, `validateMove`
	goes from this:
	```
	validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
    validateMove b p w fm = case connects w b fm of
      Right _ -> case straight w of
                   Right _ -> case firstMoveTouchesCentre w fm of
				                Right _ -> case lettersAvailable w p b
                                Left e -> Left e
               Left e -> Left e
     Left e -> Left e
	```
	to this:
	```
	validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Evaluator Bool
    validateMove b p w fm = 
	   connects w b fm 
	   >> straight w 
	   >> firstMoveTouchesCentre w fm 
	   >> lettersAvailable w p b
	```
  + There are a lot of validation functions in the previous version that checked some 
    boolean condition and returned `Right ()` if it was true or `Left` with an error message if
	it was false. The type of these is now `Evaluator ()`. The code that checks the condition
	and returns a value in the `Evaluator` monad is moved into a helper function,
	`Scrabble.Evaluator.evalBool`, saving us quite a few keystrokes: 
	```
	-- | Test a bool in the monad
    evalBool :: Bool -> String -> Evaluator ()
    evalBool b e = if b then pure () else fail e

	```
	The validation functions now have the form:
	```
	validateSomething :: ...args... -> Evaluator ()
	validateSomething args = <expression that returns a bool>
	                         `evalBool` "Descriptive error message"
	```
## Miscellaneous improvements to make it actually playable

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

## AI

+ **`v-0.5`**:
  + An AI is added, so games can be played against the computer. In
    order to achieve this, a list of playable positions is maintained
    as part of the game state. This is updated after each word is
    played -- each new word adds new playable positions but also may
    reduce the playable space around existing playable positions or
    remove them entirely. The AI player chooses one a playable position in a
    very basic way, not currently trying to get the highest score. It then
    tries to make a word with their tiles that begins or ends with
    that letter and fits in the available space.
	
	*Work in progress*: The AI would be much better if it were more flexible about
	choosing where to play. At the moment it can only play perpendicularly to en existing word.
	It could play words that by adding letters
	to the beginning or end of existing ones, and could play words with the playable position
	somewhere in the middle.
	
  + Because the module `Scrabble.Game.AI` needs to share a lot of code with `Scrabble.Game.Game`,
    common code is moved in its own module, `Scrabble.Game.Internal`. A similar change is made
	to the `Board` code, adding `Scrabble.Board.Internal`.
	
	Files in the library:
	
	```
	src
    ├── Scrabble
    │   ├── Board
    │   │   ├── Bag.hs
    │   │   ├── Board.hs
    │   │   ├── Bonus.hs
    │   │   ├── Internal.hs
    │   │   └── Validation.hs
    │   ├── Evaluator.hs
    │   ├── Game
    │   │   ├── AI.hs
    │   │   ├── Game.hs
    │   │   ├── Internal.hs
    │   │   └── Validation.hs
    │   ├── Lang
    │   │   ├── Dict.hs
    │   │   ├── Letter.hs
    │   │   ├── Search.hs
    │   │   └── Word.hs
    │   ├── Show.hs
    │   └── Types.hs
    └── Scrabble.hs

	```
+ **`v-0.6`**:
  + A web interface is added, using websockets and the `aeson` library to handle encoding and 
    decoding JSON. Datatypes that need to be sent to the client are made into instances of
    `ToJSON` and `FromJSON`. See `web/server` and `web/client`.


