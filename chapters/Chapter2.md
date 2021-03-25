# Chapter Two: Players and the game

[Contents](../README.md)

Now we can move on to think about **players** and the **game** itself. A
player has a **name**, a **rack** and a **score**. 

The name is stored as `Data.Text` rather than `String`. Wherever
possible, when we need to store some text we will use the `Text`
datatype instead of `String`. This is because `String`, being a simple
linked list, is very inefficient. Like `Data.Map`, it is usual practice to
import `Data.Text` with a qualified name, apart from the name of the
type itself which is imported directly for convenience.

```haskell
import Data.Text (Text)
import qualified Data.Text as T

-- | A player has a name, a rack and a score.
data Player = Player { name  :: Text -- ^ The name of the player.
                     , rack  :: Rack -- ^ The rack.
                     , score :: Int  -- ^ The score.
                     } deriving (Show, Eq)
```
To make working with `Text` easier, we turn on the `OverloadedStrings`
extension in our code. This means that any literal strings in our code
are treated as `Text`.  The extension is turned on in the `cabal`
config file and by including a "language pragma" (an instruction to
the compiler) at the top of any files that need it:

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
data Game = Game { board     :: Board    -- ^ The board
                 , bag       :: Bag      -- ^ The bag.
                 , player1   :: Player   -- ^ Player 1.
                 , player2   :: Player   -- ^ Player 2.
                 , turn      :: Turn     -- ^ Which player's turn it is.
                 , gen       :: StdGen   -- ^ The StdGen for all things random.
                 , firstMove :: Bool     -- ^ Is it the first move?
                 , dict      :: DictTrie -- ^ The dictionary.
                 }
```


Note that the game includes a `StdGen`, or generator for pseudo-random
numbers. We need this because we want to supply players with tiles
taken at "random" from the bag, something that we'll come to in the
next chapter.

## Records, their clumsiness, and *lenses*

As the `Player` and `Game` datatypes are records we can create them with
named fields and update them by assigning those fields inside braces. The
compiler screates an accessor function for each field with the same name
as the field.

```
> let p = Player {name = "Bob", rack = [A, B, C, D, E, F, G], score = 0}
> name p
"Bob"
> p { name = "Alice", score = 42 }
Player
    { name = "Alice"
    , rack =
        [ A
        , B
        , C
        , D
        , E
        , F
        , G
        ]
    , score = 42
    }
```

Our main record will be `Game` and it has two `Player` values nested within it. As soon
as we want to update values in this structure we encounter a well-known problem -- the
syntax for records makes this awkward. Let's say we have a game called `g` and we want 
to increase the score of Player 1 by 10:

```
> let g' = g { player1 = (player1 g) { score = score (player1 g) + 10 } }
```

Ouch! Considering that in an OO language we could probably do
something like `p.player1.score += 10`, this is very cumbersome. This
is the problem that *lenses* overcome.

Lenses are first class getters and setters for records (and tuples,
and many other types, but we're only using them for records). They can
be composed, so they allow us to access and modify values that are
*deeply nested* values in data, like the `score` field above. 

This isn't the place for an in-depth lens tutorial, and I'm aiming to
tell you just enough about them to understand the way they're used in
this project, which is very basic. It's highly recommended that you
do read such a tutorial eventually, such as the [standard
one](http://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html).

Each lens comes with two main functions: `view`, which gives the value
of the field, and `over`, which modifies its value. Rather than using
these functions by name we normally use one of the lens operators. If
we define lenses for `Player` and `Game` and use one of the standard
lens librarys, we can rewrite the the code above like this:

```
> let g' = g & player1 . score %~ (+10)
```

It's a lot cleaner, even if it is gobbledegook for now. Let's break it
down. 

The point is to produce a new `Game` based on `g` but in which the
partially applied function `(+10)` has been applied to the `score`
field nested inside `player1`. 

In this context, `score` is *not* the accessor function we saw before,
it's a lens. The `(&)` operator is like `($)` but it takes its
arguments in reverse order, so this is the same as writing `(player1
. score %~ (+10)) g`. So `g` is applied to a function which is made of
up two lenses, `player1` and `score`, composed with the usual
composition operator, `(.)`.  Then comes the `(%~)` operator, which
takes a lens as its first argument and a function as its second, and
supplies the value from the lens to the function. The modifier
function is `(+10)`.  Haskell is still a purely functional language of
course, so no change is made to `g`, but a new `Game` record is
produced which we assign to `g'`.

Lenses can be used to access the value of the field or to change
it. Which purpose the lens serves depends on the context, which is set
by the lens operators involved. For example, the lens `player1` acts
like a getter in `g ^. player1`. It acts like a setter in `g & player1
.~  p` (setting `player1` in `g` to some new value `p`).

The `(&)` operator has a very simple type, `(&) :: a -> (a -> b) -> b`, 
but is icredibly useful. We use it to supply the object at the top
of the chain (`g` in the example above) but because a record update
returns a new record we can also use it to chain updates. In the code
below (taken from the library) we start with a game called `g`,
set the score of Player 1 to some value `p1s`, set Player 2's score to
`p2s` and set `gameOver` to `True`, all (seemingly) in one go:

```haskell
g & player1 . score .~ p1s 
  & player2 . score .~ p2s 
  & gameOver .~ True 
```

We are going to use the `Lens.Simple` library, so we add it to the
cabal dependencies. We could define our own lenses for each field in
the `Player` and `Game` records, *or* we can use the `makeLenses` 
function and have them made for us. To do this, we name each field
beginning with an underscore followed by the name we want for the lens
and cast the magic spell `$(makeLenses ''RecordName)` beneath the definition.

```haskell
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
                 , _playable  :: Playable -- ^ The map of playable positions.
                 , _lastMovePass :: Bool  -- ^ Was the last move a pass?
                 }
-- | Make lenses for Game.
$(makeLenses ''Game)
```

As we can tell from the `$` sign, the call to `makeLenses` is Template
Haskell. This is the language extension that provides *metaprogramming*. We
need to include the pragma `{-# LANGUAGE TemplateHaskell #-}` at the
top of each file that has an expression like this in it, and add
`TemplateHaskell` to the list of language extensions being used in the
config file. When the code is compiled the preprocessor runs and
generates code for us. As a result there are lens functions defined with the
name of each field, minus the underscores. We need to export these
from the `Scrabble.Types` module and import them wherever they are
needed.

Lens libraries define a lot (a *lot*!) of operators like the `(%~)`
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
one or two places, then the complexity lenses add may not be worth
it. We are going to make enough use of nested records for our codebase
to benefit from them though. Also, if you carry on using Haskell you
will need to read other people's code that uses lenses as they are
very widely used, so you need to understand how they work at some
point.

The best way to get started with them is to use them, and you don't
need to understand them inside out to do that (but you do need to read
a good tutorial thoroughly at some point). Like many things in
Haskell, they can seem difficult to understand at first but it's not
brain surgery, just types.

## Testing

## Exercises

[Contents](../README.md) | [Chapter Three](Chapter3.md)
