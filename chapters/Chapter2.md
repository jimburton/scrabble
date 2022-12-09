# Chapter Two: Players and the game

[Contents](../README.md)

Now we can move on to think about **players** and the **game**
itself. A player has a **name**, a **rack**, a **score** and might
be a human or an **AI** player. A game will have two players, a **board**, a 
`StdGen` for pseudo-randomness requirements, of which more anon, and several Boolean fields 
to keep track of the progress of the game. Most of this code is going 
into a the module `Scrabble.Game`.

We introduce two record types, `Player` and `Game`, that encapsulate 
everything we need to know to manage the state of games. The types themselves
are simple but we need to take a bit of a digression to explain the
way we will working with them.

The name of each player is stored as `Data.Text` rather than
`String`. Wherever possible, when we need to store some text we will
use the `Text` datatype instead of `String`. This is because `String`,
being a simple linked list, is very inefficient. Like `Data.Map`, it
is usual practice to import `Data.Text` with a qualified name, apart
from the name of the type itself which is imported directly for
convenience.

```haskell
-- in Scrabble.Types

import qualified Data.Text as T
import           Data.Text (Text)

-- | A player has a name, a rack and a score.
data Player = Player { _name  :: Text -- ^ The name of the player.
                     , _rack  :: Rack -- ^ The rack.
                     , _score :: Int  -- ^ The score.
                     } deriving (Show, Eq)
```
The reason the field names begin with an underscore will be explained
below. 

To make working with `Text` values easier we turn on the
`OverloadedStrings` extension in our code. This means that any literal
strings in our code are treated as `Text`.  The extension is turned on
in the `cabal` config file and by including a "language pragma" (an
instruction to the compiler) at the top of any files that need it:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

Now we have enough types to create the `Game` datatype. This needs to
store everything we need to know in order to play the game (including
whose turn it is, so we create a datatype for that too). We also need
to know whether this is the first move because a special rule applies
to that (it must touch the centre square). Here is the first version,
which we'll need to add to as we uncover new requirements.

```haskell
data Turn = P1 | P2 deriving (Show, Eq)

-- | A game is comprised of all the state that is needed to play a game. 
data Game = Game { _board     :: Board    -- ^ The board
                 , _bag       :: Bag      -- ^ The bag.
                 , _player1   :: Player   -- ^ Player 1.
                 , _player2   :: Player   -- ^ Player 2.
                 , _turn      :: Turn     -- ^ Which player's turn it is.
                 , _gen       :: StdGen   -- ^ The StdGen for all things random.
                 , _firstMove :: Bool     -- ^ Is it the first move?
                 , _dict      :: DictTrie -- ^ The dictionary.
                 }
```

Note that the game includes a `StdGen`, or generator for pseudo-random
numbers. We need this because we want to supply players with tiles
taken at "random" from the bag, something that we'll come to in the
next chapter.

## Records, their clumsiness, and lenses

As the `Player` and `Game` datatypes are records we can create them with
named fields and update them by assigning those fields inside braces. The
compiler creates an accessor function for each field with the same name
as the field.

```
> let p = Player { _name = "Bob", _rack = [A, B, C, D, E, F, G], _score = 0}
> _name p
"Bob"
> p { _name = "Alice", _score = 42 }
Player
    { _name = "Alice"
    , _rack =
        [ A
        , B
        , C
        , D
        , E
        , F
        , G
        ]
    , _score = 42
    }
```

Our main record will be `Game` and it has two `Player` values nested within it. As soon
as we need to update values in this structure we encounter a well-known problem -- the
syntax for records makes this awkward. Let's say we have a game called `g` and we want 
to increase the score of Player 1 by 10:

```
> let g' = g { _player1 = (_player1 g) { _score = _score (_player1 g) + 10 } }
```

Oof! Haskell is meant to be elegant...considering that in an OO
language we could probably do something like `g.player1.score += 10`,
this is very cumbersome. This is the problem that *lenses* overcome.

Lenses are first class getters and setters for records (and tuples,
and many other types, but we're only using them for records). They can
be composed, so they allow us to access and modify values that are
deeply nested in data, like the `_score` field above. 

This isn't the place for an in-depth lens tutorial, and I'm hoping to
tell you just enough about them to understand the way they're used in
this project, which is very basic. It's highly recommended that you
do read such a tutorial eventually, such as the [standard
one](http://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html).

Each lens comes with two main functions: `view`, which gives the value
of the field, and `over`, which modifies its value. Rather than using
these functions by name we most often use one of the lens operators. If
we define lenses for `Player` and `Game` with the same names as the
fields and use one of the standard lens librarys, we can rewrite the
the code above like this:

```
> let g' = g & player1 . score %~ (+10)
> g' ^. (player1 . score)
52
```

It's a lot cleaner, even if the meaning of the operators might be
gobbledegook for now. Let's go through it bit by bit.

As we can see from the differences in their names, `score` and
`player1` are not the accessor functions we saw before. They are
lenses. Given a record, `player`, with a field, `_score`, we can get the
value of `_score` with `player ^. score` and set it to a new value,
`x`, with `player & score .~ x`. The other main thing we want to do is
to update the value of `_score` by applying a function to it, say `foo`. This
looks like this: `player & score %~ foo`.

The `(&)` operator is like `($)` but it takes its arguments in reverse
order, so our original lens function,

```haskell
g' = g & player1 . score %~ (+10)
```

is the same as writing `(player1 . score %~ (+10)) g`.  So `g` is
supplied as the parameter to a function which is a lens made up of 
`player1` and `score` composed with the usual composition operator, `(.)`.  
Then comes the `(%~)` operator, which takes a lens as its first argument and a
function as its second, and supplies the value from the lens to the
function. Haskell is still a purely functional language of course, so
no change is made to `g`, but a new `Game` record is produced which we
assign to `g'`.

Lenses can be used to access the value of the field or to "change"
it. Which purpose the lens serves depends on the context, which is set
by the lens operators involved. For example, `player1` acts like a
getter in `g ^. player1`. It acts like a setter in `g & player1 .~  p`
(producing a new game in which `player1` is set to some new value
`p`).

The `(&)` operator has a very simple type, `(&) :: a -> (a -> b) -> b`, 
but is incredibly useful. We use it to supply the object at the
top of the chain (`g` in the example above) in a readable
left-to-right way, but because a record update returns a new record we
can also use it to chain updates. In the code below (taken from the
library) we start with a game called `g`, set the score of Player 1 to
some value `s1`, set Player 2's score to `s2` and set `gameOver` to
`True`, all (seemingly) in one go:

```haskell
g & player1 . score .~ s1 
  & player2 . score .~ s2 
  & gameOver .~ True 
```

Each lens has a type similar to this one for the `_name` field of `Player`:

```haskell
name  :: Lens' Player Text
```

The first type parameter to `Lens'` is the type of the record, the second is the name of the field.
The `Lens'` type is a very crafty type synonym that allows us to use different functors depending
on whether we want to get or set fields. It speaks to the power of Haskell's type system that this
is even possible (in most languages, it isn't).

```haskell
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
           │ │
           │ └──── the type of the value inside of the structure
           │
           └───── the type of the whole structure
```

To write `name` ourselves we could have written a getter and
a setter then used the built-in `lens` function to combine them.

```haskell
getName :: Player -> Text
getName (Player { _name = n }) = n

setName :: Player -> Text -> Player
setName p n = p { _name = n }

name :: Lens' Player Text
name = lens getName setName
```
But doing this for every field is a lot of boilerplate code -- typing with no 
real thought required. As we'll see we can get tools to do for us.

We are going to use the `Lens.Simple` library, so we add it to the
cabal dependencies. Rather than defining our own lenses for each field
in the `Player` and `Game` records, we use the `makeLenses`
function. To do this, we name each field beginning with an underscore
followed by the name we want for the lens and cast the magic spell
`$(makeLenses ''RecordName)` beneath the definition.

```haskell
-- In Scrabble.Types

-- | A player has a name, a rack and a score, and is either an interactive or an AI player.
data Player = Player { _name  :: Text -- ^ The name of the player.
                     , _rack  :: Rack -- ^ The rack.
                     , _score :: Int  -- ^ The score.
                     } deriving (Show, Eq)
-- | Make lenses for Player
$(makeLenses ''Player)

-- | A game is comprised of all the state that is needed to play a game. 
data Game = Game { _board     :: Board    -- ^ The board
                 , _bag       :: Bag      -- ^ The bag.
                 , _player1   :: Player   -- ^ Player 1.
                 , _player2   :: Player   -- ^ Player 2.
                 , _turn      :: Turn     -- ^ Which player's turn it is.
                 , _gen       :: StdGen   -- ^ The StdGen for all things random.
                 , _firstMove :: Bool     -- ^ Is it the first move?
                 , _dict      :: Dict     -- ^ The dictionary.
                 , _gameOver  :: Bool     -- ^ Is the game over?
                 }
-- | Make lenses for Game.
$(makeLenses ''Game)
```

The `$` character indicates that the call to `makeLenses` is Template
Haskell. This is the language extension that provides
*metaprogramming* ("code that writes code"). We need to include the
pragma `{-# LANGUAGE TemplateHaskell #-}` at the top of each file that
has an expression like this in it, and add `TemplateHaskell` to the
list of language extensions being used in the config file. When the
code is compiled, a preprocessor runs and generates all of those
definitions for us. As a result there are lens functions defined with
the name of each field, minus the underscores. We need to export these
from the `Scrabble.Types` module and import them wherever they are
needed.

We refactor `updateBoard` to work over `Game` rather than `Board` and
to use the `lens` approach.

```haskell
-- | Place a word onto the board.
updateBoard :: WordPut -> Game -> Game
updateBoard w g = g & board .~ foldl updateSquare (g ^. board) w
```

Lens libraries define a lot (a *lot*!) of operators like the `(.~)`
one used above, and when you get them wrong the type errors are hard
to figure out, at least at first. We are going to stick to a small number
of the most basic operators though:


| Operator | Name   | Example                                                                     |
| -------- | ------ | --------------------------------------------------------------------------- |
| `(^.)`   | view   | `g ^. player1`: gets Player 1 in `g`.                                       |
| `(.~)`   | set    | `g & player1 . score .~ 0`: sets the score of Player 1 to 0.                |
| `(%~)`   | over   | `g & player1 . name %~ map T.toUpper`: applies `map T.toUpper` to the name. |
| `(&)`    | apply  | Reverse application, used for supplying the first record to a composed lens, and for chaining operations |

If you aren't using nested records in your code, or you only do so in
one or two places, you may be thinking right now that the extra
cognitive burden of lenses isn't worth the convenience they bring. I
think this is normally only the case for people who haven't used them
though; they are a straightforward idea with some clever use of types
behind it which can admittedly be a bit daunting at first. We are certainly
going to make enough use of nested records to benefit greatly from
them anyway. They are very widely used so if you carry on using
Haskell you'll need to read other people's code that uses them. So
you'll need to read a good tutorial thoroughly at some point but, for
now, just go ahead and use them. Like most things in Haskell that can
seem difficult to understand at first, it's just types.

## Creating a game

To start a fresh game we need to create a full bag then two players,
each with a rack that has been filled with tiles taken at "random"
from the bag. Then the two players and the depleted bag are added to
the game state. Creating a full bag is easy enough.

```haskell
-- in Scrabble.Bag

-- The distribution of each tile that is in a new bag.
numTilesList :: [(Letter,Int)]
numTilesList = [
  (A, 9), (B, 2), (C, 2), (D, 4), (E, 12), (F, 2), (G, 3),
  (H, 2), (I, 9), (J, 1), (K, 1), (L, 4), (M, 2), (N, 6),
  (O, 8), (P, 2), (Q, 1), (R, 6), (S, 4), (T, 6), (U, 4),
  (V, 2), (W, 2), (X, 1), (Y, 2), (Z, 1), (Blank, 2) ]

-- | A new bag containing a full set of tiles.
newBag :: Bag
newBag = concatMap (\(l,n) -> replicate n l) numTilesList
```

For the randomness, we are going to use a pseudo-random number
generator (PRNGs). These are completely deterministic
(i.e. non-renadom) data structures. They are created using a *seed*,
and can produce a stream of values whose sequence is hard enough for
humans to predict that it appears to be truly random. But there's
nothing magical happening -- PRNGs created with the same seed return
the same stream of values, and ones in the same state (i.e. in the
same position in its stream of values) returns the same value
next. 

Every time we use the PRNG it returns the latest value and an updated
version of itself, primed to return the next value. So our function
that fills a rack needs to take a rack to be filled, a bag to fill the
rack from and a PRNG as parameters, and return a triple of the filled
rack, the depleted bag and, crucially, the updated PRNG. The type for
PRNGs that we'll be using is `StdGen`. The function `randomR (n,m) g`
takes an upper and lower bound and a `StdGen` then returns a number
between `n` and `m` and the updated `StdGen`.

```haskell
-- | Refill a rack with tiles picked randomly from the bag. Returns the filled
--   rack, the new bag and the updated random generator.
fillRack :: Rack   -- ^ The rack to fill 
         -> Bag    -- ^ The bag to pick from.
         -> StdGen -- ^ The random generator.
         -> (Rack, Bag, StdGen) -- ^ The filled rack, the updated bag and the updated StdGen.
fillRack r b g = pure $ fillRack' (7 - length r) r b g
  where fillRack' 0 r' b' g' = (r', b', g')
        fillRack' _ r' [] g' = (r', [], g')
        fillRack' n r' b' g' =
          let (t, b'', g'') = getTile b' g' in
            fillRack' (n-1) (t:r') b'' g''

-- Get a single tile from a bag.
getTile :: Bag    -- The bag to take the tile from.
        -> StdGen -- The random generator.
        -> (Letter, Bag, StdGen) -- A letter from the bag, the updated bag and the updated StdGen.
getTile b g = let (i, g') = randomR (0, length b -1) g
                  t = b !! i
                  b' = take i b ++ drop (i+1) b in
              (t, b', g')
```

When we start a game we need to begin with a new `StdGen`. We can get
one created with a seed based on the system time using `getStdGen ::
IO StdGen` then keep updating it throughout the game. Because we don't
want everything in our library to be polluted with `IO` we leave the
call to `getStdGen` to clients and presume they can supply one to the
`newGame` function below. Reading in the dictionary file is also an
`IO` action that we presume is done elsewhere.

```haskell
-- in Scrabble.Game

-- | Start a new game.
newGame :: Text   -- ^ Name of Player 1
        -> Text   -- ^ Name of Player 2
        -> StdGen -- ^ The random generator
        -> Dict   -- ^ The dictionary
        -> Game
newGame p1Name p2Name theGen d = 
  let (rack1, bag1, gen') = fillRack [] newBag theGen
      p1 = Player { _name  = p1Name
                  , _rack  = rack1
                  , _score = 0
                  , _isAI = False }
      (rack2, bag2, gen'') = fillRack [] bag1 gen'
      p2 = Player { _name  = p2Name
                  , _rack  = rack2
                  , _score = 0
                  , _isAI  = False }
      g  = Game { _board     = newBoard
                , _bag       = bag2
                , _player1   = p1
                , _player2   = p2
                , _turn      = P1
                , _gen       = gen''
                , _firstMove = True
                , _dict      = d
                , _gameOver  = False
                , _lastMovePass = False } in
    g
``` 

Note that we updated the `Game` type to include two new boolean
fields, `_gameOver` and `_lastMovePass`. These relate to the two ways
in which a game can end.  We will come back to this when we start
actually playing the game.

## Testing

To test functions involving games we need to be able to create new
(arbitrary) games. This involves `IO` because we need to supply a
dictionary and a `StdGen` to the `newGame` function. 

The module `Test.Chapter2` adds some tests relating to bags and
filling racks. As the `fillRack` function requires a `StdGen` as a
parameter, clearly this has to do some `IO`. We can achieve this using
functions from `Test.QuickCheck.Monadic` such as `monadicIO` then
`liftIO` when we want to run an `IO` action.


```haskell
-- | Test that using @fillRack@ takes the tiles
--   from the bag and puts them in the rack.
prop_fillRack1 :: Property 
prop_fillRack1 = monadicIO $ do
  g <- liftIO getStdGen
  let b = newBag
      (r',b',_) = fillRack [] b g
  assert $ (length r' == 7) && (length b' == length b - 7)

```

## Exercises

+ In `Scrabble.Game`, write a function to return the lens for the
  player whose turn it currently is. To make this type check you need
  to add the Language pragma `Rank2Types` to the top of the file.

  ```haskell
  getPlayer :: Game -> Lens' Game Player
  ```
+ Use `getPlayer` to write a function that uses lens operators to add
  a value to the score of the current player.
  
  ```haskell
  addToCurrentPlayerScore :: Game -> Int -> Game
  ```

[Contents](../README.md) | [Chapter Three](Chapter3.md)
