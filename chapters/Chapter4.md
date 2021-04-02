# Chapter Four: Playing the game

[Contents](../README.md)

We now have enough resources to start playing the game. Not actually
with a user interface -- what we mean by "playing" is using the REPL
to generate an initial game state then calling functions to "take
turns" that generate new game states.

Code that takes turns repeatedly until the game is over is not part of
this section. That functionality is the responsibility of clients,
because it will necessarily involve IO, and the desired response when
something goes wrong will be different for each client. All the
library knows how to do is to use pure functions to produce a fresh
game given some starting conditions, and to produce a new game based
on an existing one, i.e. by playing a move.

We will deal first with the simplest way of playing a move, which is
by *passing*. Then we consider the almost equally simple case of
taking a move by *swapping tiles*, before looking at the "normal" way
of taking a move by *playing a word* on the board.

## Passing a move

A player can take a move by passing their turn. The rule is that if
there are two passes in the row then the game is ended, so now we need
to think about how to end games. All the library will do to end a game
is to make some changes to the score then set the `gameOver` field of
the game to `True`. We will leave it up to clients to check this value
and make an announcement to the players if necessary.

The `lastMovePass` field of the game is there to detect the situation
where there have been two passes in a row. We set it to `True` when a
player passes and to `False` when they take a move in any other
way. When there are two passes in a row we pass the game to `endGame`,
which sets the `gameOver` field and finalises the scores. Remaining
tiles in each player's rack are subtracted from their score, and if
one player has used all of their tiles, the tiles in the other
player's rack are added to their score.

The other way that the game can end is if we run out of tiles in the
bag and one player has used all of the tiles in their rack. This is
checked in `checkEndOfGame`, which also calls `endGame` if necessary.

```haskell
-- in Scrabble.Board.Board

rackValue :: Rack -> Int
rackValue = sum . map scoreLetter

-- in Scrabble.Game.Game

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

A player can take a move by swapping some tiles from their rack for
new ones from the bag. We take the tiles to swap from the rack, refill
the rack, then add the old tiles from the rack back to the bag. The
`endNonPassMove` function manages the state of the fields `firstMove`
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
the normal way. It will need a `Validator` to heck the move, a game,
and a word to play. If all goes well it will return an updated game and a
pair of a list of all new words and the score. Like the other ways of playing a
move it will run in the `Evaluator` monad, so failure at any point is
handled nicely.

We'll write this top level function then fill in
the parts that it needs to do its work.

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
  same as `(<$>)` but with the order of the arguments reversed:
  
  ```haskell
 (<&>) :: Functor f => f a -> (a -> b) -> f b 
  ```
  
  So it takes a functor (the game, wrapped up in the `Evaluator`
  functor) and a pure function as it's second argument, then applies
  that function within the functor. The pure function uses a *tuple
  section* -- the triple `(,(map wordPutToWord (w:aw),sc))` is missing
  its first component and is a partially applied function "waiting"
  for that part. This requires a language extension, `TupleSections`,
  which is turned on at the top of the module and added to the `cabal`
  file. We could always have written this part of the code like this:
  
  ```
  >>= checkEndOfGame >>= \g' -> pure (g',(map wordPutToWord (w:aw),sc))
  ```
 
## In the REPL

We've now made just about all of the building blocks of the
library. You can experiment with the current code in the REPL like
this:

```
$ cabal repl 
> :m + Scrabble
> :m System.Random
> theGen <- getStdGen
> d <- englishDictionary
> let g = newGame "Bob" "Alice" theGen d
> player1 g
Player
    { name = "Bob"
    , rack =
        [ E
        , I
        , O
        , A
        , P
        , C
        , V
        ]
    , score = 0
    , isAI = False
    }
```
We create a `WordPut` from the tiles in Bob's rack and play the first move,
which has to touch the centre square:

```
> let w = [((7,7),(P,3)), ((7,8),(I,1)), ((7,9),(E,1))]
> let (Ev (Right (g2,(ws,sc)))) = move valGameRules g w []
λ> ws
[
    [ P
    , I
    , E
    ]
]
> sc
10
```
We take a move as Alice:

```
> player2 g2
Player
    { name = "Alice"
    , rack =
        [ V
        , Z
        , Y
        , T
        , E
        , Blank
        , H
        ]
    , score = 0
    , isAI = False
    }
> let w2 = [((7,7),(P,3)), ((8,7),(E,1)), ((9,7),(T,1))]
> let (Ev (Right (g3,(ws,sc)))) = move valGameRules g2 w2 []
> showBoard False (board g3)
"  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 5|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 6|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 7|  |  |  |  |  |  |  | P| I| E|  |  |  |  |  |
 8|  |  |  |  |  |  |  | E|  |  |  |  |  |  |  |
 9|  |  |  |  |  |  |  | T|  |  |  |  |  |  |  |
10|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
11|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
------------------------------------------------
"
```
We swap some tiles:

```
> turn g3
P1
> player1 g3
Player
    { name = "Bob"
    , rack =
        [ M
        , N
        , S
        , O
        , A
        , C
        , V
        ]
    , score = 10
    , isAI = False
    }
> let (Ev (Right g4)) = swap [M,N,S,O] g3
> player1 g4
Player
    { name = "Bob"
    , rack =
        [ O
        , E
        , N
        , N
        , A
        , C
        , V
        ]
    , score = 10
    , isAI = False
    }
> turn g4
P2
```
Finally we pass the move:

```
> let (Ev (Right g6)) = pass g5
> turn g6
P1
```
Let's try playing some bad moves:

```
> turn g6
P1
> player1 g6
Player
    { name = "Ted"
    , rack =
        [ M
        , N
        , S
        , O
        , A
        , C
        , V
        ]
    , score = 10
    , isAI = False
    }
```
First, a move for which we don't have the tiles:

```
> let notInRack = [((5,7),(D,3)), ((5,8),(D,1)), ((5,9),(D,1))]
> let (Ev (Left e)) = move valGameRules g2 notInRack []
> e
"Not all tiles in rack or on board: DDD: (5,7) HZ"
```
One that isn't played on the board in a straight line:

```
> let notStraight = [((7,10),(S,1)), ((8,10),(O,1)), ((9,11),(N,1))]
> let (Ev (Left e1)) = move valGameRules g2 notStraight []
> e1
"Not in a straight line"
```
One that doesn't touch any existing tile on the board:

```
> let notConnected = [((8,10),(S,1)), ((9,10),(O,1)), ((10,11),(N,1))]
> let (Ev (Left e2)) = move valGameRules g2 notConnected []
> e2
"Not touching any other tile"
```
One that isn't in a continuous line:

```
> let notContinuous = [((7,10),(S,1)), ((8,10),(O,1)), ((10,10),(N,1))]
> let (Ev (Left e43)) = move valGameRules g2 notContinuous []
λ> e3
"Not in a straight line"
```
Note that we aren't distinguishing between being straight and being continuous.
We could fix that by refactoring `straight` into `straight` and a new check,
`continuous`, if we thought it mattered. 

Finally, using a different validator, we try to play a word which is all OK except 
that it doesn't exist:

```
> let notAWord = [((7,10),(S,1)), ((8,10),(V,4)), ((9,10),(N,1))]
> let (Ev (Left e4)) = move valGameRulesAndDict g2 notAWord []
> e4
"Not in dictionary: SVN"
> 
```

In the next chapter we'll make it possible to play against the computer.

## Tests

TODO

## Exercises

[Contents](../README.md) | [Chapter Five](Chapter5.md)
