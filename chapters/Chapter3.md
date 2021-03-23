# Chapter Three: Playing the game

The code corresponding to this version of the project is in the branch 
`chapter3`.

We now have enough resources to start playing the game. There is no UI
as yet, and what we mean by "playing" is using the REPL to generate an
initial game state then calling funcitons to "take turns" that generate
new game states. 

Code that takes turns repeatedly until the game is over is not part of
this section. That functionality is the responsibility of clients,
because the desired response when something goes wrong will be
different for each client. All the library knows how to do is to
produce a new game and play a single move within an existing game.

## Setting up the game

To start a game we need to create a full bag then two players, each with
a rack that has been filled with tiles taken at random from the bag. Then
the two players and the depleted bag are added to the game state.

Pseudo-random number generators (PRNGs) in Haskell are
deterministic. They are created using a *seed*, and PRNGs created with
the same seed return the same values. Every time we use the PRNG it
returns the latest value and an updated version of itself, primed to
return the next value. So our function that fills a rack needs to
take a rack to be filled, a bag to fill the rack from and a PRNG as
parameters, and return a triple of the filled rack, the depleted bag
and the updated PRNG. The type for PRNGs is `StdGen`. The function
`randomR (n,m)` returns a random number between `n` and `m`.

```haskell
getTile :: Bag -> StdGen -> (Letter, Bag, StdGen)
getTile b g = let (i, g') = randomR (0, length b -1) g
                  t = b !! i
                  b' = take i b ++ drop (i+1) b in
              (t, b', g')
			  
fillRack :: Rack -> Bag -> StdGen -> Evaluator (Rack, Bag, StdGen)
fillRack r b g = pure $ fillRack' (7 - length r) r b g
  where fillRack' 0 r' b' g' = (r', b', g')
        fillRack' _ r' [] g' = (r', [], g')
        fillRack' n r' b' g' =
          let (t, b'', g'') = getTile b' g' in
            fillRack' (n-1) (t:r') b'' g''
```

When we start a game we need to begin with a new PRNG. We can get one
created with a seed based on the system time using `getStdGen :: IO
StdGen` then reuse it throughout the game. Because we don't want
everything in our library to be polluted with `IO` we leave the call
to `getStdGen` to clients and presume they can supply one to the
`newGame` function below. Reading in the dictionary file is also an
`IO` action that we presume is done elsewhere.

```haskell
numTilesList :: [(Letter,Int)]
numTilesList = [
  (A, 9), (B, 2), (C, 2), (D, 4), (E, 12), (F, 2), (G, 3),
  (H, 2), (I, 9), (J, 1), (K, 1), (L, 4), (M, 2), (N, 6),
  (O, 8), (P, 2), (Q, 1), (R, 6), (S, 4), (T, 6), (U, 4),
  (V, 2), (W, 2), (X, 1), (Y, 2), (Z, 1), (Blank, 2) ]

newBag :: Bag
newBag = concatMap (\(l,n) -> replicate n l) numTilesList

newGame :: Text -> Text -> StdGen -> Dict -> Game
newGame p1Name p2Name theGen d = 
  let Ev (Right (rack1, bag1, gen')) = fillRack [] newBag theGen
      p1 = Player { name  = p1Name
                  , rack  = rack1
                  , score = 0 }
      Ev (Right (rack2, bag2, gen'')) = fillRack [] bag1 gen'
      p2 = Player { name  = p2Name
                  , rack  = rack2
                  , score = 0 }
      g  = Game { board     = newBoard
                , bag       = bag2
                , player1   = p1
                , player2   = p2
                , turn      = P1
                , gen       = gen''
                , firstMove = True
                , dict      = d 
				, gameOver  = False 
				, lastMovePass = False} in
    g
``` 

Note that we updated the `Game` type to include two new booleans, `gameOver`
and `lastMovePass`. These relate to the two ways in which a game can end. 
Normally the game ends when the bag is empty and one of the
players has used all of their tiles. ALternatively, the game is over if each
player passes their move. 

We will deal first with the simplest way of playing a move, which is
by passing. Then we consider the almost equally simple case of taking
a move by swapping tiles, before looking at the "normal" way of taking
a move by playing a word on the board.

## Passing a move

A player can take a move by passing their turn. If there are two
passes in the row then the game is ended, so now we need to think
about how to end games. All the library will do to end a game is to
make some changes to the score then set the boolean `gameOver` to
`True`. We will leave it up to clients to check this boolean and make
an announcement to the players.

The `lastMovePass` boolean is there to detect the situation where
there have been two passes in a row. We set it to `True` when a player passes
and to `False` when they take a move in any other way. When there are
two passes in a row we pass the game to `endGame`, which sets
`gameOver` and calculates the final scores. Remaining tiles in each
player's rack are subtracted from their score, and if one player has
used all of their tiles, the tiles in the other player's rack are
added to their score.

The other way that the game can end is if we run out of tiles in the bag. This
is checked in `checkEndOfGame`, which also calls `endGame` if necessary.

```haskell
rackValue :: Rack -> Int
rackValue = sum . map scoreLetter

endGame :: Game -> Evaluator Game
endGame g = do
  let r1v = rackValue (rack (player1 g))
      r2v = rackValue (rack (player2 g))
      p1s = (score (player1 g) - r1v) + r2v
      p2s = (score (player2 g) - r2v) + r1v
  pure g { player1 = (player1 g) { score = p1s }
         , player2 = (player2 g) { score = p2s }
         , gameOver = True }
		 
checkEndOfGame :: Game -> Evaluator Game
checkEndOfGame g =
  let eog = null (bag g) &&
        (null (rack (player1 g)) || null (rack (player2 g))) in
  if eog
  then endGame g
  else pure g

toggleTurn :: Game -> Evaluator Game
toggleTurn g = pure g { turn = if turn g == P1 then P2 else P1 }

pass :: Game -> Evaluator Game
pass g = if lastMovePass g
         then endGame g
         else toggleTurn g { lastMovePass = True } >>= checkEndOfGame

```
## Swapping tiles

A player can take a move by swapping some tiles from their rack for new ones from the 
bag. We take the tiles to swap from the rack, refill the rack, then add the swapped tiles 
to the bag. The `endNonPassMove` function manages the state of the boolean fields `firstMove`
and `lastMovePass`.

```haskell
endNonPassMove :: Game -> Evaluator Game
endNonPassMove g = toggleTurn $ g { firstMove = False, lastMovePass = False }

swap :: Word -> Game -> Evaluator Game
swap ls g = do
  let p      = getPlayer g
      r      = rack p
      theBag = bag g
      theGen = gen g
  takeFromRack r ls >>= \r' -> fillRack r' theBag theGen
    >>= \(r'', theBag', theGen') -> setPlayer g (p { rack = r'' })
    >>= endNonPassMove >>= checkEndOfGame
    >>= \g' -> pure g' { bag = ls++theBag'
                       , gen = theGen'
                       , lastMovePass = False }

```

## Playing a move

Now we need to write a function that plays a word onto the board in
the normal way. It will need a `Validator`, a game, and a word to
play. It will run in the `Evaluator` monad so failure at any point is
handled nicely, and if all goes well it will return an updated game, a
list of all new words and the score. We'll write this top level
function then fill in the parts that it needs to do its work.

```haskell
move :: Validator -> Game -> WordPut -> Evaluator (Game, ([Word],Int))
move validate g w = additionalWords g w >>= \aw -> validate (w:aw) g
    >> scoreWords g w aw >>= \sc -> setScore g sc 
    >>= updatePlayer >>= updateBoard w >>= endNonPassMove 
	>>= checkEndOfGame <&> (,(map wordPutToWord (w:aw),sc))
```

There is a lot going on here! We won't go through every part in
detail, but you should study the code in `src/Scrabble/Game.hs`. At
each stage the game state is being updated or some other value is
being calculated, such as the score. Some things to note:

+ Because the `Validator` is a parameter we can call `move` with
  `valGameRules` only if we don't want to include a dictionary check,
  or `valGameRulesAndDict` to check everything.
+ To see how the score is calculated, study the functions `scoreWord`
  and `scoreWords` in `Scrabble.Board`. (*Disclaimer*: these functions
  are unpleasantly messy, but this seems to be unavoidable.) 
  
  Every tile in a new word adds its face value multiplied by the
  letter-bonus of the square it is placed on, if any. Then the score
  for the whole word is multiplied by any word-bonus that a new tile
  has been placed on. A `Map` called `bonusMap` is maintained of the
  positions of bonus squares on the board. Bonus squares can double or
  triple the value of the tile played on them (shown in the ASCII
  board as `L2` and `L3`) and double or triple the value of entire
  words (`2W` and `3W`). However, bonuses are only applied once, when
  a new tile has been placed on them. If all seven tiles are played in
  a move there is a fifty point bonus.
  
+ The `checkEndOfGame` function produces a game (which might be
  updated to say that this is the end of the game). We pass this game
  to the `(<&>)` operator from `Data.Functor`.  This operator is the
  same as `(<$>)` but with the order of the arguments reversed. The
  pure function uses a *tuple section* -- the triple is "waiting" for
  its first part. This requires a language extension, `TupleSections`,
  which is turned on at the top of the module and added to the `cabal`
  file. We could always have written this part of the code like this:
  
  ```
  >>= checkEndOfGame >>= \g' -> pure (g',(map wordPutToWord (w:aw),sc))
  ```
 
## Tests
 TODO
[Contents](../README.md) | [Chapter Four](Chapter4.md)
