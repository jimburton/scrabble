# Chapter 1: Getting started

[Back](../README.md)

Code corresponding to this chapter is in the branch `chapter1`.

In this chapter we will explain the initial state of the project,
create some basic datatypes that allow us to model the game, and some
simple functions for placing words on boards and so on.

## The project

We're using `cabal` to manage the project. `cabal` deals in
*libraries* and *executables*. We will be making several executables
later but the core code for playing the game is contained in a
library. Since it is a library, there isn't an entry point or any way
for users to run the code. It is intended for ourselves and others to
import it into code that *is* intended for users.

The library is defined in this stanza in the config file:

```
library
  hs-source-dirs: src
  ghc-options: -Wall
  build-depends: base >=4.12 && <4.13
               , random
               , array
               , containers
               , text
               , text-trie
  
  exposed-modules:
      Scrabble
      Scrabble.Types
      Scrabble.Game

  default-extensions: OverloadedStrings
  
  default-language: Haskell2010
```

The `hs-source-dirs` property tells `cabal` where to find the
code. Take a look at the `exposed-modules` property. These are the
modules in our library and currently there are three of them:
`Scrabble`, `Scrabble.Types` and `Scrabble.Game`.  These are defined
in the files `src/Scrabble.hs`, `src/Scrabble/Types.hs` and
`src/Scrabble.Game.hs`.

The `Scrabble` module just imports and re-exports the entire library. The
`Scrabble.Game` module contains the functions we'll be describing in this
chapter, while the `Scrabble.Types` module contains the datatypes and type
aliases. 

It is a fairly common Haskell idiom to put the datatypes into a 
module of their own -- it makes them easy to track down and can help you
avoid the problem of circular imports, where two modules need to import each
other to get access to some types, something which isn't allowed by the compiler.

To experiment with the code, use the command `cabal repl`. You still need to load
the modules containing types and functions:

```
$ cabal repl
> :m + Scrabble.Types
> :m + Scrabble.Game
```


## Building blocks of the game

When you start writing any software you need to think about modelling the problem
in hand. When the problem is a board game, this is easy to do because the first things
we need to model in the software correspond to real world objects.

![Source: https://www.argos.co.uk](/images/scrabble.jpeg)

In the image above we can see the most basic objects we will need to handle:

+ a number of **tiles**,
+ several **racks** containing tiles,
+ a **bag**, also containing tiles, and last but not least
+ the **board**.

We also need to account for some things that we can't see such as a
**game**, which consists of the objects above plus several **players**
and some additional state (e.g., whose **turn** it is). Each player
will have a **score**, as well as rack. Finally, we also need to model
the **rules** of the game and a **dictionary**.

## Tiles, letters and words

![Source: https://www.ebay.co.uk](/images/tile.jpg)

A tile consists of a **letter** and a **value**. We could create a type alias such
as

```haskell
type Tile = (Char,Int)
```

but then the type system wouldn't be able to rule out nonsense values
like `('%',1)`, we might find ourselves needing to distinguish between
'a' and 'A', and so on. So we create an enumeration of all possible
letters and make the datatype derive some useful typeclasses:

```haskell
data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank
  deriving (Show, Read, Enum, Eq, Ord)

type Tile = (Letter,Int)

```
We will need several datasets relating to letters. We need to know

+ the score of each letter,
+ how many of them should be in a full bag, and
+ the `Char` value for printing.

We will store this data in *maps*, using the `Data.Map` API. Maps are
efficient (`O(log n)`) lookup tables.  `Data.Map` is normally imported
with a qualified name (e.g. `Map`) since it contains many functions
whose names clash with those of functions in the `Prelude`.


```haskell
import qualified Data.Map as Map

-- lookup table for the score of a letter 
letterToScoreList :: [(Letter,Int)]
letterToScoreList = [
  (A, 1), (B, 3), (C, 3), (D, 2), (E, 1), (F, 4), (G, 2),
  (H, 4), (I, 1), (J, 8), (K, 5), (L, 1), (M, 3), (N, 1),
  (O, 1), (P, 3), (Q, 10), (R, 1), (S, 1), (T, 1), (U, 1),
  (V, 4), (W, 4), (X, 8), (Y, 4), (Z, 10), (Blank, 0) ]

-- map to find the score of a letter 
letterToScoreMap :: Map Letter Int
letterToScoreMap = Map.fromList letterToScoreList

-- | Find the score of a letter.
scoreLetter :: Letter -> Int
scoreLetter = fromJust . flip Map.lookup letterToScoreMap

``` 

Of these functions, the only one we want to export is
`scoreLetter`. Good information hiding and encapsulation is key to
creating software that is nice to work on. Note the use of `fromJust`
in `scoreLetter`. This is an unsafe function, meaning that it can fail
at runtime causing the program to crash. This happens when it is called
on a value that isn't `Just x`, i.e. which is `Nothing`. In this case, 
it's fine because we know that there is an entry in the map for every letter 
in.

Words are lists of letters and both racks and bags are lists of tiles.

```haskell
type Word = [Letter]

type Rack = [Tile]

type Bag = [Tile]
```
## The board

A board consists of positions within rows and columns -- we will model
the board as a two-dimensional array. This will certainly be a lot
more efficient than a list of lists. The values stored in the array
will be `Maybe Tile`s (i.e. either `Nothing` for an empty square or
something like `Just (A,1)` for a square with an 'A' tile on it).

```haskell
import Data.Array

type Board = Array (Int,Int) (Maybe Tile)

```
Then, if we have a board called `b`, we can access the value in row `r`
column `c` by `b ! (r,c)`. These `(r,c)` pairs are going to be used a lot,
so we make a type for those too.

```haskell
type Pos = (Int,Int)
```

A word we want to place on the board is a list of pairs of positions on the
boards and tiles.

```haskell
type WordPut = [(Pos, Tile)]
```
Last up for the board, it has **bonus squares**. These are either double or 
triple *word* bonuses or double or triple *letter* bonuses. We make a datatype for 
bonuses and a map of their positions.

```haskell
data Bonus = W2 | W3 | L2 | L3
  deriving Show
  
bonusSquaresList =
  [((0, 0),    W3), ((0, 3),   L2)
  , ((0, 7),   W3), ((0, 11),  L2)
  , ((0, 14),  W3), ((1, 1),   W2)
  , ((1, 5),   L3), ((1, 9),   L3)
  ...
  
bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList
```

## The dictionary

A copy of the standard English Scrabble dictionary as a text file with
one word per line is stored at `dict/en.txt`. It is a big file, with
more than 260,000 entries. Obviously we need to store this in a data
structure which is as efficient as possible.

If we only ever wanted to look up words to see if they exist then a 
hashtable would be the best choice, with search taking `O(1)`. However,
we want to build a computer player at some point, so we will need 
efficient ways of finding words based on any collection of letters,
words that fit with existing tiles on the board, and so on. 

There are several data structures that store words (or any sequence of
values) in ways that allow prefixes to be shared. The
[trie](https://en.wikipedia.org/wiki/Trie) allows us to find a word
and all of its prefixes very quickly (`O(m)`, where `m` is the length of
the word). Here is trie storing the words *the*, *their*, *there*,
*answer*, *any* and *bye*:

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

We don't actually need to store anything at the leaves, we just need to know
whether which paths in the trie exist.

```haskell
type Dict = Trie ()
```

Other options are the Suffix Tree and Directed Acyclic Word Graph. Both
of these use less space than a trie but the Haskell package for tries has
more features.

Now we can create the dictionary and check whether a word exists as
follows:

```haskell
readDictionary :: FilePath -> IO Dict
readDictionary dict = do
  ls <- lines <$> readFile dict
  return (Trie.fromList [(pack (map toUpper l), ()) | l <- ls])

dictContainsWord :: Dict -> Text -> Bool
dictContainsWord = flip Trie.member 

```

## Players and the game

Now we can move on to think about players and the game itself. A
player has a name a rack and a score. Wherever possible, we are going
to use the `Text` datatype instead of `String` when we need to store
text content. This is because `String` is inefficient. Like
`Data.Map`, it is usual practice to import `Data.Text` with a
qualified name, apart from the name of the type itself which we import
directly for convenience.

```haskell
import Data.Text (Text)

data Player = Player { name  :: Text
                     , rack  :: Rack
                     , score :: Int
                     } deriving (Show, Eq)

```

To make working with `Text` easier, we turn on the `OverloadedString`
extension in out code. This means that any literal strings in our code
is treated as `Text`.  The exyension is turned on in the `cabal`
config file and by including a "language pragma" (an instruction to
the compiler) at the top of any files that need it:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

Now we can create the `Game` type. This needs to store everything we
need to know in order to play the game (including whose turn it is, so
we create a datatype for that too). Here is the first version, which
we'll add to as we uncover new requirements.

```haskell
data Turn = P1 | P2 deriving (Show, Eq)

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
taken at "random" from the bag.

## Putting a word on the board

To create the initial empty board we can use the `array` function to
turn a list of `Nothing` values into a 15x15 array. Then we can put a
wordput onto the board.

```haskell
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

But we can't print it out nicely yet. Let's look at that next. We want a function that
turns a board into a formatted text. To do that we will need to deal with a row at
a time from the array, which is what the `rows` function is for. It gets the contents
of the array as a list then uses `chunksOf` to split it into 15-element sublists. The
`intercalate` function from `Data.Text` intersperses a lists of texts with its
argument. The `(<>)` operator concatenates text.

```haskell
import qualified Data.Text as T
import Data.Array
import Data.List.Split (chunksOf)

rows :: Board -> [[Maybe Tile]]
rows b = chunksOf 15 (elems b) 

-- | Textify a board.
showBoard :: Bool  -- ^ Whether to show bonus squares.
          -> Board -- ^ The board.
          -> Text
showBoard printBonuses b = topNumbers <> top <> showRows <> bottom where
  showRows      = T.intercalate "\n" (zipWith showRow [0..14] (rows b)) <> "\n"
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

