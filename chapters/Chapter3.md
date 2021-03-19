## The first version

+ **`v-0.1`**: This branch contains the initial effort. It includes
  the basic data types (`Letter` , `Word`, `Board`, `Bag`, `WordPut`,
  which is a word along with its position on the board, `Player`,
  `Game` and so on). There are functions for starting a game, taking
  turns in which players put words onto the board, and showing the
  board and the score. The dictionary is held as a `Set` of `Text`
  values. There is no UI but you can start a game and take a few moves
  in the REPL like this:
  
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
	needs. 
	
	The UI code is moved into the directory `cli/`. Source files in the project:
	
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
	cli
    └── Main.hs

	```
  + To play the game you now need to enter `cabal repl scrabble`.
  + Use of the `haskeline` library to enable using backspace and arrow keys in the CLI.
    Note that this changes to the `takeTurn` function in `cli/Main.hs`.

## Optimising the data structures and making good use of functional design patterns.

+ **`v-0.3`**:
  + The dictionary is now stored in a [trie](https://en.wikipedia.org/wiki/Trie), which is a very
    efficient data structure for storing strings sorted by their prefixes. This speeds up the
    process of checking whether words exist quite a bit.
  + The `String` datatype is used all over the place in earlier
    versions. In this version this is almost all replaced with
    `Data.Text`, which is much more efficient. `Data.Text` contains a
    lot of function names which clash with names in the `Prelude` so
    the norm is to import it as a qualified name, except for the
    `Text` datatype itself which is imported directly:
	
	```
	import qualified Data.Text as T
	import Data.Text (Text)
	```
  + The biggest change in the library is the introduction of monadic `Either` code for handling 
	errors. A new type called `Evaluator` is added (see `Scrabble.Types` and `Scrabble.Evaluator`). 
	This type wraps up an `Either String a` type, where the `String` is an error message and the 
	`a` value is whatever is being evaluated (e.g. an updated version of the game, or just `()` 
	in cases where moves are validated). By making this type into a monad we are able to replace all of
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
	The functions that check aspects of the move are now *combinators*. We can compose
	them into larger combinators that check several things. If `connects`, or `straight`, 
	or any of the other checks made in the monadic version fails
	by returning a `Left` with an error message in it, this is handled by the monad instance
	declaration. Functions at the top level can inspect the result of calling a chain of 
	computations to see if all went well. For example, the `takeTurn` function in `cli/Main.hs`
	calls `move`, which now returns `Evaluator Game`. This is unwrapped to decide whether to 
	move on to the next turn or to ask the current player to try again:
	
	```
	case move valGameRules g wp is of
	    -- there was an error, retake the turn with the old game
        Ev (Left e) -> do liftIO $ T.putStrLn e
                          liftIO $ takeTurn g $ Just (T.pack wd  <> ": NO SCORE")
        -- all is well, take the next turn with the new game
        Ev (Right (g',sc)) -> liftIO $ takeTurn g' (Just (T.pack $ show sc))
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

## AI

+ **`v-0.5`**:
  + An AI is added, so games can be played against the computer. In
    order to achieve this, a list of *playable* positions is
    maintained. A playable position is one where the AI could play a
    word, so we need to know the letter at that position, the amount
    of space around it and the direction of that space. Several new
    types are added to support this.
	
	A `FreedomDir` is a direction on the board -- `Upd`, `DownD`,
    `LeftD` or `RightD`.
	
	A `Freedom` is a `FreedomDir` and a distance. 
	
	Then we create a map with the type `Map Pos (Letter, [Freedom])`, which is added
	to the game state and updated after each move is played. When the AI comes to make a move
	it needs to repeatedly take a playable position and try to create a word that can be played
	against it. If the direction of the freedom is `UpD` or `LeftD` the AI needs to find a word
	that *ends* with the letter in question. If the direction is `RightD` or `DownD` it needs to
	find a word that *begins* with the letter in question. In each case the freedom tells the AI 
	the maximum length of the word. The functions that search for words are in `Scrabble.Lang.Search` 
	and `Scrabble.Lang.Dict` and use the API of the `Trie` datatype.
	
	The freedoms map needs to be updated after each word is
    played -- each new word adds new playable positions but also may
    reduce the playable space around existing playable positions or
    remove them entirely. The figure below shows the freedoms on the board after
	the first move. The freedom of one of the positions with a tile on it is shown: 
	`[(LeftD, 7), (RightD, 7)]`. In this case, all of the playable positions have the same freedom.
	
    ![](/images/freedoms0.png)
	
	Note that it would be possible to make a legal move by extending the word with a prefix or suffix.
	For instance, playing tiles to make the word `FOULED`, or `BEFOUL` or even putting tiles before and
	after the word to make `BEFOULED`. The AI currently makes no attempt to do this. Nor does it
	try to get a high score! This could be done by modifying `Scrabble.Game.AI`, especially the
	`findWord` function and the functions it depends on.
	
	![](/images/freedoms1.png)
	
	The figure above shows what happens after more tiles are placed on
	the board. Several freedoms have been removed (too many, in
	fact -- see below). This takes place in the function
	`Scrabble.Game.Internal.updatePlayables`.
	
	*Work in progress*: The AI would be much better if it were more flexible about
	choosing where to play. At the moment it can only play perpendicular to an existing word.
	It could play words that by adding letters
	to the beginning or end of existing ones, and could play words with the playable position
	somewhere in the middle. It could also be more careful about pruning playable positions. 
	In the example above the position `(7,7)` which has the letter 'F' on it *is* playable, 
	but is currently removed from the list for simplicity. It could also put up more of a fight 
	by searching for the best move, but smarter strategies would be needed to do this in reasonable 
	time. These strategies could include trying to make words using high value tiles and 
	which are placed on bonus tiles.
	
  + Because the module `Scrabble.Game.AI` needs to share a lot of code with `Scrabble.Game.Game`,
    common code is moved into its own module, `Scrabble.Game.Internal`. A similar change is made
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
