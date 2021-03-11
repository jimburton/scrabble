
# Scrabble

An multiplayer implementation of Scrabble for teaching functional programming at the 
University of Brighton.

The Scrabble library is adapted from https://github.com/joshcough/Scrabble, while the UI 
borrows code from https://github.com/danielweck/scrabble-html-ui.

The emphasis is on refining an initial solution to a clean software design that can easily
be extended. The evolution of the code is demonstrated in branches that students should study 
in this order:

+ `v-0.1`: This branch contains the initial effort. It includes the basic data types (`Letter`
  , `Word`, `Board`, `Bag`, `WordPut`, `Player`, `Game`) and functions for starting a game, taking
  turns in which players put words onto the board and showing the board and the score. The 
  dictionary is held as a `Set` of `Text` values. There is no UI but you can start a game and take 
  a few moves in the REPL like this:
  
  ```
  $ git checkout v-01
  $ cabal repl
  $ startGame "Bob" "Alice"
  **********************************************
  Bob (0)
  A, T, F, H, E, D, A
  1, 1, 4, 4, 1, 2, 1
  **********************************************


  **********************************************
  Alice (0)
  W, O, D, E, E, Q, V
  4, 1, 2, 1, 1, 10, 4
  **********************************************

  Turn: P1
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

  **********************************************
  Alice (0)
  W, O, D, E, E, Q, V
  4, 1, 2, 1, 1, 10, 4
  **********************************************
  Enter WORD ROW COL DIR[H/V]:
  wode 11 5 h
  Turn: P1
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

  **********************************************
  Bob (22)
  V, E, H, T, T, H, A
  4, 1, 4, 1, 1, 4, 1
  **********************************************
  Enter WORD ROW COL DIR[H/V]:
  ```
  The code lives in the `src` directory.
  
  **Things to look out for in the code:**
  
  + The `Main` module contains the `startGame` function and the loop that plays the game.
	The use of `getLine` in that loop means that if you try to use the backspace
	key when entering a move it will show up as garbage so type carefully or you will have
	to quit the game with `Ctrl-C` and start again.
  + The datatypes live in the module `Scrabble.Types`.
  + The main function for playing the game is
    `Scrabble.playGame`. Moves are validated there by functions whose
    names begin `validate...`q. They each return
    `Either String a` where the String is an error message and `a` may
    be any type.  After each validation function is called we need to check whether the result
	is an error message (`Left e`) or a "good" result (`Right x`).
  + It is convenient to switch off the dictionary checking in
	development so that you don't have to come up with a valid word
	every time you want to enter a move. You can switch the dictionary
	checking off or on by changing the call to the function
	`validateGameRulesAndDict` in `Scrabble.playGame` to
	`validateGameRules`.
  + There is no way to check whether the game has ended or who won.
  
+ `v-0.2`:
  + The code is split into a library (in the directory `src`),
    that contains all the code for managing a game of Scrabble, and an
    executable (in the directory `cli`), which contains the
    command-line interface (CLI).  This will make it easy to add
    subsequent UIs, such as a web-based one or a mobile app. Study the `cabal` config file
	to see how this is organised.
  + To play the game you now need to enter `cabal repl scrabble`.
  + Code is in the library is reorganised. See the modules...

Explanation of Evaluator monad, with this example of nasty nested case statements:

```
-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated as being in the dictionary.
move :: Dict    -- ^ The dictionary
     -> Game    -- ^ The game
     -> WordPut -- ^ The word to play
     -> Bool    -- ^ Is first move
     -> Either String (Game, Int)
move d g w fm =
  let b   = board g
      p   = getPlayer g
      aw  = additionalWords b w 
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      waw = map (map snd) (w:aw)
      fpb = if newTilesInMove b w == 7 then 50 else 0
      sc  = sum $ map (scoreWord fpb) sws in 
  case validateMove b p w fm of
    Right _ -> case wordsInDict d waw of
      Right _ -> let g' = setScore g sc in
                 Right (g' {board = updateBoard b w}, sc)
      Left e -> Left e
    Left e -> Left e

{- Non-monadic version of validateMove for comparison
-- | Check that a move is valid: it touches at least one existing word (unless
--   it is the first move, in which case check that it touches the centre square),
--   it is in a straight and continuous line, and is made
--   up of letters that either in the rack or on the board.
validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
validateMove b p w fm = case connects w b fm of
  Right _ -> case straight w of
               Right _ -> if all (\(pos,t) -> case getSquare b pos of
                                     Just l  -> l == t
                                     Nothing -> t `elem` rack p) w
                          then if fmGood
                               then Right True
                               else Left "First move must touch centre square"
                          else Left "Letters not in rack or not on board"
               Left e -> Left e
  Left e -> Left e
  where fmGood = not fm || touches (7,7) w 
-}

```
Use gridNeighbours from Board to talk about higher orderness and succinct code.
