s# Chapter One: Getting started

[Contents](../README.md)

## Who this book is for, and what it is for

This book was written for students on the *Introduction to
Functional Programming* course at the University of Brighton, or anyone
else at the same stage of learning. The contents are introduced after
students have learned the basics of Haskell, written quite a few small
functions, learned about recursion, folds, practised a little bit of
IO and heard about typeclasses like `Functor` and `Monad`. They have
used `cabal` a little bit.

There is a notoriously big gap between learning the basics of Haskell
listed above, which mostly means a lot of fiddling about with lists,
and finding yourself making useful, nicely written and idiomatic
software with Haskell. For beginners the language can seem academic
and removed from the tasks we normally want to achieve with
programming languages. When they look at Haskell code in the wild it
bears very little resemblance to their exercises.

This book is meant to go some way towards filling that gap. By the end
you'll have been introduced to some major and widely used libraries,
sophisticated data structures, functional design patterns, and means
for structuring a Haskell project so that you can keep on top of it
as it grows.

I'm reluctant to say it will take you from being a beginner-level to an
intermediate-level Haskell programmer; Haskell is a deep language and if
*Intermediate* is halfway between *Beginner* and *Expert* then that's
a bit of a stretch. But you won't be a beginner any more.

## Reading this book

Throughout this book the code in the **Scrabbλe** project will be
developed incrementally. We will be adding to it, removing from it and
improving it as we go. There is a branch in the repository for each
chapter, named `chapter1`, `chapter2`, etc, up to `chapter7` (the code
for Chapter 8 is the same as that in the `main` branch). As you read
each chapter you should check out the corresponding branch and study
the code. There are exercises at the end of each chapter which expect
you to be working on the code from the right branch.

## The project

We're using `cabal` to manage the project. `cabal` deals in
*libraries* and *executables*. We will eventually be making several
executables but the core code for playing the game is contained in a
library. As such, there isn't an entry point or any way for users to
run the code. It is there for ourselves and others to import into
code that *does* provide an interface for users.

The library is defined in this stanza in the config file:

```
library
  hs-source-dirs: src
  ghc-options: -Wall
  build-depends: base >=4.12 && <4.17
               , random
               , array
               , containers
               , text
               , text-trie
  
  exposed-modules:
      Scrabble
      Scrabble.Types
	  Scrabble.Board
	  Scrabble.Dict

  default-extensions: OverloadedStrings
  
  default-language: Haskell2010
```

The `hs-source-dirs` property tells `cabal` where to find the
code. Take a look at the `exposed-modules` property. These are the
source files in our library at this stage. 

```
src/
├── Scrabble
│   ├── Board.hs
│   ├── Bonus.hs
│   ├── Dict.hs
│   ├── Game.hs
│   ├── Pretty.hs
│   └── Types.hs
└── Scrabble.hs
```

The `Scrabble` module just imports and re-exports the entire
library. The `Scrabble.Types` module contains all of our datatypes and
type aliases.

It is a fairly common Haskell idiom to put the datatypes into a module
of their own. It makes them easy to track down and can help you avoid
the problem of circular imports, where two modules need to import each
other to get access to some types, something which isn't allowed by
the compiler.  As the projects gets bigger, it's handy to know just
where to look for the definition of a type.

To experiment with the code, use the command `cabal repl`. You still
need to load the modules containing types and functions. As all of these 
are re-exported by the `Scrabble` module, just import that.

```
$ cabal repl
> :m + Scrabble
```

The code includes `haddock` style comments, so one way to browse the code would 
be to build the docs and view them in your browser. When you run `cabal haddock`
it tells you where it has stored the output:

```
$ cabal haddock
...
Documentation created:
<path-to-docs>/jb-scrabble/index.html
```

However, only those functions and types that are exported by a module
have `haddock` comments, so read the source code to see the whole
thing. Each module exports only what other modules need and imports
only what it needs. Good information hiding and encapsulation is key
to creating software that is nice to work on.

## Building blocks of the game

When you start writing any software you need to think about modelling
the problem in hand. When the problem is a board game, this is quite
easy to begin with because the first things we need to model in the
software correspond to real world objects.

<img src="/chapters/images/scrabble.jpeg" alt="Scrabble board" width="500px" />

In the image above we can see many of the most basic objects we will
need to handle:

+ the **board**.
+ a number of **tiles**,
+ several **racks** containing tiles, and
+ a **bag**, also containing tiles.


We also need to account for some things that we can't see such as the
**game** itself, which consists of the objects above plus several
**players** and some additional state (e.g., whose **turn** it
is). Each player will have a **score**, as well as rack. Finally, we
also need to model the **rules** of the game and a **dictionary**.

## Letters and tiles

<img src="/chapters/images/tile.jpg" alt="Scrabble tile" width="100px" />

A tile consists of a **letter** and a **value**. We could create a type alias such
as

```haskell
type Tile = (Char,Int)
```

but then the type system wouldn't be able to rule out nonsense values
like `('%',0)`, we might find ourselves needing to distinguish between
'a' and 'A', and so on. So we create an enumeration of all possible
letters and make the datatype derive some useful typeclasses:

```haskell
-- in Scrabble.Types

-- | Letters.
data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Show, Read, Enum, Eq, Ord)

-- | A tile is a pair of a letter and a value.
type Tile = (Letter,Int)

```
We will need to know a number of things about letters:

+ the score of each letter,
+ how many of each letter should be in a full bag, and
+ the `Char` value for printing.

We will store this data in *maps*, using the `Data.Map` API. We could
use a list of pairs with the type `[(Letter,Int)]` but maps are much
more efficient (`O(log n)`) lookup tables.  

Because it contains many functions whose names clash with those of
functions in the `Prelude`, like `filter` and `map`, `Data.Map` is
normally imported with a qualified name (e.g. `Map`) like this:

```haskell
import qualified Data.Map as Map
```
Since `Data.Map` isn't in the `Prelude`, we need to tell `cabal` where
to find it. If we try to import it without doing anything else, `cabal` 
will prompt us to add the package that includes it, `containers`, to 
the `build-depends` section of the `cabal` file. We will do that, 
and you should bear in mind that this is done whenever importing 
non-`Prelude` types in future. Often `cabal` will be able to tell us 
the name of the package to add but if not, use google to find the type 
you need on hackage and check which package it is part of.

```haskell
-- in Scrabble.Board

import qualified Data.Map as Map

-- lookup table for the score of a letter. Not exported.
letterToScoreList :: [(Letter,Int)]
letterToScoreList = [
  (A, 1), (B, 3), (C, 3), (D, 2), (E, 1), (F, 4), (G, 2),
  (H, 4), (I, 1), (J, 8), (K, 5), (L, 1), (M, 3), (N, 1),
  (O, 1), (P, 3), (Q, 10), (R, 1), (S, 1), (T, 1), (U, 1),
  (V, 4), (W, 4), (X, 8), (Y, 4), (Z, 10), (Blank, 0) ]

-- map to find the score of a letter. Not exported.
letterToScoreMap :: Map Letter Int
letterToScoreMap = Map.fromList letterToScoreList

-- | Find the score of a letter. Safe.
scoreLetter :: Letter -> Int
scoreLetter = fromJust . flip Map.lookup letterToScoreMap

``` 

Note the use of `fromJust` in `scoreLetter`. The `fromJust` function
is *unsafe*, meaning that it can fail at runtime causing the program
to crash. This happens when it is called on a value that isn't `Just x`, 
i.e. which is `Nothing`. In this case, we know we won't get any
errors because there is an entry in the map for every letter, so the
`scoreLatter` function is safe. But whenever you use an unsafe
function such as `fromJust` or `head`, ask yourself whether this is
definitely safe to do.

## The board

A Scrabble board is a 15x15 matrix of rows and columns, so a natural way to
model it is as a two-dimensional array. The values stored in
the array will be `Maybe Tile`s (i.e. either `Nothing` for an empty
square, or something like `Just (A,1)` for a square with an 'A' tile
on it). 

In many languages we would create an array of arrays to achieve this,
where each element of the 15-element outer array is a 15-element array
representing a row. However, Haskell supports true multi-dimensional
arrays, so we can create one where the type of indices is `(Int,Int)`
(for our purpose, `(row,column)`). The `Array` type constructor takes
two arguments, the type of indices and the type of elements.

```haskell
-- in Scrabble.Types

import Data.Array

-- | The board, a 2D array of Maybe letters and their scores.
type Board = Array (Int,Int) (Maybe Tile)

```
Then, if we have a board called `b` we can access the value in row `r`,
column `c`, by `b ! (r,c)`. These `(r,c)` pairs are going to be used a lot
so we make a type for those too.

```haskell
-- | A position on the board.
type Pos = (Int,Int)
```
## Words 

*Words* are lists of letters and both *racks* and *bags* are lists of tiles.
Because the `Prelude` includes a type called `Word` we have a name clash here.
We could call it `ScrabbleWord` or something like that, but it seems more 
convenient to keep the short name and hide the type in the `Prelude`, which
we don't need anyway.

```haskell
import Prelude hiding Word

-- | A word is a list of letters. 
type Word = [Letter]

-- | A rack is a list of letters.
type Rack = [Tile]

-- | The bag is a list of letters.
type Bag = [Tile]
```
A word we want to place on the board is a list of pairs of `Pos` and `Tile` values. 
We'll call this a `WordPut`.

```haskell
-- | A word placed on the board (tiles plus positions).
type WordPut = [(Pos, Tile)]
```

Last up for the board are the **bonus squares**. These are either
double or triple word bonuses, or double or triple letter bonuses. We
make a datatype for bonuses and a map of their positions. We will put
everything other than the type for bonuses in its own module to keep
things tidy.

```haskell
-- in Scrabble.Types

-- | The datatype of bonuses on the board.
data Bonus = W2 | W3 | L2 | L3
  deriving Show

-- in Scrabble.Bonus

bonusSquaresList =
  [((0, 0),    W3), ((0, 3),   L2)
  , ((0, 7),   W3), ((0, 11),  L2)
  , ((0, 14),  W3), ((1, 1),   W2)
  , ((1, 5),   L3), ((1, 9),   L3)
  ...
  
bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList
```

## Dealing with blanks

When a blank tile is played the player nominates a letter that the blank should
stand for and it keeps that value for the rest of the game. The blank
contributes zero to the score.

There are several ways we could deal with blanks in the game. We could
store blanks on the board like normal tiles and keep a map of
positions and letters (`Map Pos Letter`) to lookup the letters blanks
stand for. We choose to store a `Tile` with the letter the blank
stands for on the board, with its score set to zero. Clients will take
care of interrogating players for the letters to use when they play a
blank. 

This approach has the advantage that after being played the tile is
treated like any other. A disadvantage is that it means we have to
store the whole tile -- the letter and its score -- on the board,
rather than just storing the letter and looking up its score when we
need it. But it means we don't need to check for and accomodate blanks
on the board in a lot of code that we'll write later.


## The dictionary

A copy of the standard English Scrabble dictionary as a text file with
one word per line is stored at `dict/en.txt`. It is a pretty big file, with
more than 260,000 entries. Obviously we need to store this in a data
structure which is as efficient as possible, especially when it comes to 
being searched.

If we only ever wanted to look up words to see if they exist then a
hashtable would be the best choice, with search taking `O(1)`
time. However, we want to search in more flexible ways than this.
We are going to build a computer player at some point, so we
might want to find all words that can be made based on a collection of
letters, words that include existing tiles on the board, all words
that are prefixes of some other word and so on.

There are several data structures that store words (or any sequence of
values) in ways that allow prefixes to be shared. This not only saves
a lot of space but allows the flexibility in searching that we
need. The [trie](https://en.wikipedia.org/wiki/Trie) allows us to find
a word and all of its prefixes very quickly (in `O(m)` time, where `m`
is the length of the word -- i.e. independently of `n`, the size of
the dictionary). 

Other good options for storing a dictionary of words include the
Suffix Tree and Directed Acyclic Word Graph. Both of these use less
space than a trie but the Haskell package for tries has more features
so we'll stick with that.  Here is an illustration of a trie storing
the words *the*, *their*, *there*, *answer*, *any* and *bye*, giving
you an idea of how prefixes are shared:

```
     root
   /  |   \
  t   a    b
  |   |    |
  h   n    y
  |   | \  |
  e   s  y e
 /|   |
i r   w
| |   |
r e   e
      |
      r
```

We don't actually care what is stored at the leaves of the trie, as we
only need to know which paths in the trie exist. So in each leaf we
just store `()` ("unit"), which is the type with exactly one value in
it (also called `()`, "unit").

```haskell
-- in Scrabble.Types

import Data.Trie.Text

type Dict = Trie ()
```

Now we can create the dictionary and check whether a word exists as
follows:

```haskell
-- in Scrabble.Dict

import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.Trie.Text as Trie

readDictionary :: FilePath -> IO Dict
readDictionary dict = do
  ls <- lines <$> readFile dict
  return (Trie.fromList [(T.pack (map toUpper l), ()) | l <- ls])

dictContainsWord :: Dict -> Text -> Bool
dictContainsWord = flip Trie.member 

```

We also create a number of utility functions for searching the dictionary, which
you can read in `Scrabble.Dict`. These work along the same lines as 
`dictContainsWord` but do things like finding all words that can be made with
a rack (`makeWords`), or filtering a list of words to those that are in the
dictionary (`findWords`).

## Putting a word on the board

To create the initial empty board we can use the `array` function to
turn a list of pairs of indices and `Nothing` values into a 15x15
array. Then we can put a `WordPut` onto the board. The `updateBoard`
function uses a fold to update the array with each element of the
`WordPut` in turn. The `(//)` operator is used in `updateSquare` to
update the array.

```haskell
-- in Scrabble.Board

newBoard :: Board
newBoard = array ((0,0),(14,14)) [((i,j), Nothing) | i <- [0..14], j <- [0..14]]

updateBoard :: WordPut -> Board -> Board
updateBoard w b = foldl updateSquare b w

updateSquare :: Board -> (Pos, Tile) -> Board
updateSquare b (pos,t) = b // [(pos, Just t)]

-- in a cabal repl session
> let cat = [((0,0),(C,3)),((0,1),(A,1)),((0,2),(T,1))]
> updateBoard cat newBoard

```

## Retrieving a word from the board

We can check whether a position on the board is occupied by a tile
with the following functions.

```haskell
-- | Is a position on the board?
onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r < 15 && c >= 0 && c < 15

-- | Retrieve a position on the board.
getSquare :: Board -> Pos -> Maybe Tile
getSquare b pos = if onBoard pos
                    then b ! pos
                    else Nothing
```

Each `WordPut` is placed horizontally or vertically. We make a type
for the direction of a `WordPut` and a function to calulate
directions.

```haskell
-- in Scrabble.Types

-- | A direction on the board (row or column).
data Dir = HZ -- ^ The horizontal direction.
         | VT -- ^ The vertical direction.
         deriving (Show, Read, Eq)

-- in Scrabble.Board

-- | Get direction of a word on the board. WordPuts must be at least two tiles
--   long.
getDirection :: WordPut -> Dir
getDirection w = let r1 = fst $ fst $ head w
                     r2 = fst $ fst $ head (tail w) in
                   if  r1<r2 then VT else HZ
```

Given an occupied position, if we know the direction we can find the
beginning of the `WordPut` it is part of, the start of that WordPut
retrieve the whole thing. To do so we need to transform positions by
decrementing or incrementing rows and columns. Functions that do this
will have the type `Pos -> Pos`, and we give an alias to that type,
`PosTransform`. See `Scrabble.Board` for the `incRow`, `decRow`,
`incCol` and `decCol` pos transforms.

```haskell
-- in Scrabble.Types

-- | Transform a position on the board
type PosTransform = Pos -> Pos

-- in Scrabble.Board

-- Find the starting position of a word that crosses a position on the board.
startOfWord :: Board        -- ^ The board.
            -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
            -> Pos          -- ^ The position
            -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'

-- Retrieve the word that crosses this pos on the board
wordFromSquare :: Board        -- ^ The board.
               -> PosTransform -- ^ Moves to the start of the word (up rows or left along columns).
               -> Pos          -- ^ The pos.
               -> WordPut
wordFromSquare b f pos = reverse $ wordFromSquare' pos []
  where wordFromSquare' p wp = case getSquare b p of
          Nothing -> wp
          Just t  -> wordFromSquare' (f p) ((p,t):wp)

-- | Retrieve the word that crosses a position on the board horizontally.
wordOnRow :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> WordPut
wordOnRow b pos = wordFromSquare b incCol (startOfWord b decCol pos)

-- | Retrieve the word that crosses a position on the board vertically.
wordOnCol :: Board -- ^ The board.
          -> Pos   -- ^ The position on the board.
          -> WordPut
wordOnCol b pos = wordFromSquare b incRow (startOfWord b decRow pos)
```

Now we can query the board in various ways.

```
> let cat = [((0,0),(C,3)),((0,1),(A,1)),((0,2),(T,1))]
> getDirection cat
HZ
> let b = updateBoard cat newBoard
> getSquare b (0,0)
Just
    ( C
    , 3
    )
> getSquare b (5,6)
Nothing
> wordOnRow b (0,2)
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

```
## Pretty-printing boards

Let's look at printing boards nicely next. We want a function that
turns a board into text that looks something remotely like a Scrabble
board. To do that we will need to deal with a row at a time from the
array, which is what the `rows` function below is for. It gets the
contents of the array as a list then uses `chunksOf` to split it into
15-element sublists. The `intercalate` function from `Data.Text`
intersperses a lists of texts with its argument. The `(<>)` operator
concatenates things, including text. Note that the `showBoard`
function takes a boolean parameter which determines whether to show
the bonus squares on the board. If this parameter is `True` then each
position is looked up in the map of bonus squares to see if there is a
bonus value to display.

```haskell
-- in Scrabble.Pretty

import qualified Data.Text as T
import Data.Array
import Data.List.Split (chunksOf)

rows :: Board -> [[Maybe Tile]]
rows b = chunksOf 15 (elems b) 

toChar :: Letter -> Char
toChar l = fromJust $ Map.lookup l letterToCharMap

-- | Textify a board.
showBoard :: Bool  -- ^ Whether to show bonus squares.
          -> Board -- ^ The board.
          -> Text  -- ^ The board as text. 
showBoard printBonuses b = topNumbers <> top <> showRows <> bottom where
  showRows        = T.intercalate "\n" (zipWith showRow [0..14] (rows b)) <> "\n"
  showRow     i r = showI i <> "|" <>
                    T.concat (zipWith (showSquare i) [0..14] r)
  showSquare :: Int -> Int -> Maybe (Letter,Int) -> Text
  showSquare i c s = case s of
                       Nothing    ->
                         if printBonuses
                         then case Map.lookup (i,c) bonusMap of
                                Nothing -> "  |"
                                Just b' -> T.pack (show b') <> "|"
                         else "  |"
                       Just (t,_) -> T.pack [' ', toChar t, '|']
  topNumbers    = "  |" <> T.concat (map (\i -> showI i <> "|") [0..14]) <> "\n"
  showI         :: Int -> Text
  showI i       = if i < 10 then " " <> T.pack (show i) else T.pack (show i)
  top           = line '-'
  bottom        = line '-'
  line        c = T.pack (replicate 48 c) <> "\n"
  
> showBoard False (updateBoard cat newBoard)
"
  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0| C| A| T|  |  |  |  |  |  |  |  |  |  |  |  |
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
"
> showBoard True (updateBoard cat newBoard)
"
  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0| C| A| T|L2|  |  |  |W3|  |  |  |L2|  |  |W3|
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
"
```

## Testing

Before we move on, let's make some tests. We need to think about what
we want to be always true about the types and functions we have
created. We use the `QuickCheck` library for property-based testing. This
means that we specify some property that we want our functions to have and
the library generates arbitrary input that checks whether the property
holds.

The `test-suite` stanza in the config file deptermines what tests should
be run and how. It points to the file `tests/Main.hs` as the entry point.

```
tests/
├── Main.hs
└── Test
    ├── Chapter1.hs
    └── Gen.hs
```

To test functions relating to boards we need to be able to generate
abitrary values of `Pos`, `Letter`, `WordPut` and so on. We write
generators that use the `QuickCheck` library to do that in
`Test.Gen`. For instance, here's the generator for arbitrary
tiles. Before we can define it we have to tell `QuickCheck` how to
produce an arbitrary `Letter` by defining an `Arbitrary` instance.

```haskell
instance Arbitrary Letter where
  arbitrary = chooseEnum (A,Z) 

-- | Generate an arbitrary Tile.
genTile :: Gen Tile
genTile = do
  l <- elements [A .. Z]
  pure (l, scoreLetter l)
```

Tests that use these generators are in `Test.Chapter1`.

## Exercises

TODO

[Contents](../README.md) | [Chapter Two](Chapter2.md)
