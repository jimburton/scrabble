# Chapter Three: Validating moves

[Contents](../README.md)

In this chapter we introduce a change that will have a big influence
on the internal design of the library from now on: monadic error
checking. It will allow us to thread the state of the game through a
long and growing series of computations without having to worry at
each stage about whether anything went wrong.

We are going to introduce a lot of functions that relate to validating
moves in the context of games and boards. Rather than putting all of
this code into `Scrabble.Game` and `Scrabble.Board`, we carry on
breaking it all out into separate cohesive modules.  So we create
directories `src/Scrabble/Game/` and `src/Scrabble/Board` and create
the relevant modules within them. 

While we're at it we refactor the dictionary code into code that
builds the dictionary and code that searches it, in modules within
`src/Scrabble/Lang/`. We'll put all code relating to letters and words
in this area too.

```
src/
├── Scrabble
│   ├── Board
│   │   ├── Bag.hs
│   │   ├── Board.hs
│   │   ├── Bonus.hs
│   │   ├── Pretty.hs
│   │   └── Validation.hs
│   ├── Evaluator.hs
│   ├── Game
│   │   ├── Game.hs
│   │   └── Validation.hs
│   ├── Lang
│   │   ├── Dict.hs
│   │   ├── Letter.hs
│   │   ├── Search.hs
│   │   └── Word.hs
│   └── Types.hs
└── Scrabble.hs
```

## Checking the structure and position of words

Now that we can place words onto the board, we want to check whether
doing so is valid. The rules about word placement are as follows:

+ Words are at least two letters long.
+ Words must be actually on the board.
+ A word must be continuous and either horizontal or vertical.
+ The first word must cross the centre square, while subsequent words
  must connect with an existing word.
+ The word and all additional words that it generates must be in the
  dictionary.
  
Two positions on the board, `(r1,c1)` and `(r2,c2)`, are in a
continuous straight line if either `r1==r2-1` and `c1==c2` or `r1==r2`
and `c1==c2-1`. One approach to writing `straight` would simply to
return `True` if the word is straight:

```haskell
straight :: WordPut -> Bool
straight wp | length wp > 2 = 
                let ps  = map fst wp
                (s1,s2) = if getDirection wp == HZ then (fst,snd) else (snd,fst)
                f       = \(x',y') -> s1 x' == s1 y' - 1 && s2 x' == s2 y' in 
                  (all f . zip ps $ tail ps)
            | otherwise    = False
```

But there are lots of ways in which a move might be invalid and we'd
like to be able to let the user know exactly what went wrong. If we
carry on with validation checks that return true or false we will need lots
of `if` statements that work out what to report back to the user.

A common solution to writing a function that can fail is to use the
type `Either Text a`. Values of this type are either `Left e`, where
`e` is an error message, or `Right x` where `x` is any type. In the
case of `straight`, if it returns any kind of `Right` value then we
know things went well. So there's no need to return a `Bool`, we can
just return `Left e` for an error or `Right ()` if things went
well. 

```haskell
-- in Scrabble.Board.Validation

straight :: WordPut -> Either Text ()
straight wp | length wp > 2 =
                let ps      = map fst wp
                    (s1,s2) = if getDirection wp == HZ then (fst,snd) else (snd,fst)
                    f       = \(x',y') -> s1 x' == s1 y' - 1 && s2 x' == s2 y' in 
                  if (all f . zip ps $ tail ps)
                   then Right ()
                   else Left "Not in a straight line"
            | otherwise    = Left "Too few letters"
```

To check whether a word connects with an existing word, we need to look at the
"neighbours" of each position in the new word and check whether at least one
neighbour has a tile in it. Since this only applies if this is not the first move,
we pass a boolean parameter which is true if this is the first move.

```haskell
-- in Scrabble.Board.Board

neighbours :: Pos -> [Pos]
neighbours (r,c) = filter onBoard [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

occupiedNeighbours :: Board -> Pos -> [Pos]
occupiedNeighbours b pos = filter (isJust . getSquare b) $ neighbours pos

-- in Scrabble.Board.Validation

connects :: WordPut -> Board -> Bool -> Either Text ()
connects ws b fm =
  let end = if fm then Right () else Left "Not touching any other tile" in
    foldl (\acc (pos,_) -> if (not . all null) (occupiedNeighbours b pos)
                           then Right ()
                           else acc) end ws
```
Checking that a word is on the board just means checking that every position is on
the board.

```haskell
-- | Check that a WordPut is on the board.
wordOnBoard :: WordPut -> Either Text ()
wordOnBoard w = if all (onBoard . fst) w
                 then Right ()
                 else Left "Word not on board"
```
Checking that the first word touches the centre square is also 
straightforward with the `touches` function.

```haskell
-- | Does the word touch the pos on the board?
touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

-- If this is the first move, does it touch the centre square?
firstMoveTouchesCentre :: WordPut -> Bool -> Either Text ()
firstMoveTouchesCentre w fm = if (not fm || touches (7,7) w) 
                               then Right ()
                               else Left "First move must touch the centre square"
```
We also need to check that the letters in the word to be played are actually in the player's 
rack or are already on the board.

```haskell
-- The letter in this move are available in the player's rack or on the board.
lettersAvailable :: WordPut -> Player -> Board -> Either Text ()
lettersAvailable w p b = if all available w
                          then Right ()
                          else "Letters not in rack or not on board."
  where available (pos,(t,_)) = maybe (t `elem` rack p) ((==t) . fst) (getSquare b pos)

``` 

So, we already have lots of things to check about the validity of a
move. We need to check whether the word is straight, whether it
touches another word and whether the tile are available, and this is
before we have even got onto checking the dictionary. Each of these
function calls will return an `Either Text a` and we may find
ourselves doing a lot of case statements and pattern matching on
`Either` values. A function that puts together the various ways we
migh validate move could look like this:
	
```
-- in Scrabble.Board.Validation

validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
validateMove b p w fm = 
    case wordOnBoard w of
      Right _ -> case connects w b fm of
                   Right _ -> case straight w of
                                Right _ -> case firstMoveTouchesCentre w fm of
                                             Right _ -> case lettersAvailable w p b of
                                                          Right -> Right ()
                                                          Left e -> Left e
                                             Left e -> Left e
                                Left e -> Left e
                   Left e -> Left e
      Left e -> Left e
 ```
 
The technical term for this kind of code is "nasty". Such a deeply
nested and indented structure is hard to read, hard to maintain and
hard to extend.  Fortunately, what we can do here is to use a monad to
encapsulate the checks for `Left` and `Right`. We make our `Either`
type into a monad, where the monad instance says what to do when we
encounter a `Left` value, and then when we use the monad we can carry
on as if everything is a `Right` value -- no more case statements.

We create a new type for arbitrary "evaluations" in the game, called
`Evaluator`. 

```haskell
-- in Scrabble.Types

newtype Evaluator a = Ev (Either Text a)
```
This type wraps up an `Either Text a` type where, as you
may expect, the `Text` is an error message and the `a` value is
whatever is being evaluated. For instance, `a` could be `()` in cases 
where moves are being checked for validity, or `Game` when a function 
either fails or returns an updated version of the game, or `Int` when
a function either fails or calculates the score of a word. A value of
the type `Evaluator Int` would be something like `Ev (Left "Something went wrong.")`
or `Ev (Right 42)`. 

Now we need to make a monad instance for `Evaluator`. That requires us
to first define the `Functor` and `Applicative` instances,
since every monad is an applicative and every applicative is a
functor. The spirit of these definitions is that if we are dealing
with an `Ev (Left _)` value we want to **stop what we are doing and
report the error**, while if we are dealing with a `Ev (Right _)`
value we can **keep going**.

```haskell
-- in Scrabble.Evaluator

instance Functor Evaluator where
  -- fmap :: (a -> b) -> f a -> f b 
  fmap _ (Ev (Left e))  = Ev (Left e)      -- report the error
  fmap f (Ev (Right g)) = Ev (Right (f g)) -- keep going

instance Applicative Evaluator where
  -- pure :: a -> f a
  pure k = Ev (Right k)
  -- (<*>) :: f (a -> b) -> f a -> f b
  Ev (Left  e)  <*>  _  =  Ev (Left e) -- report the error
  Ev (Right f)  <*>  r  =  fmap f r    -- keep going

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg) -- report the error
          Right v  -> k v           -- keep going
    return   = pure
    fail msg = Ev (Left (T.pack msg))
```
Now we need to rewrite all of the functions that returned `Either Text a`
to return `Evaluator a`. The ones we have seen so far tested a boolean condition,
`b`, and returned `Right ()` if `b` succeeded or `Left Text` if `b` failed. We can
make an abstraction for this pattern.

```haskell
-- in Scrabble.Evaluator

evalBool :: Bool -> Text -> Evaluator ()
evalBool b e = if b then pure () else fail (T.unpack e)
```
(Note that the `fail` function takes a `String` so we have to `unpack` the `Text`.) 
Now our validation functions will all have a similar structure to the new version 
of `lettersAvailable` below -- a call to `evalBool` where the first argument is a boolean 
condition and the second is an error message.

```haskell
lettersAvailable :: WordPut -> Player -> Board -> Evaluator ()
lettersAvailable w p b = all available w `evalBool`"Letters not in rack or not on board."
  where available (pos,(t,_)) = maybe (t `elem` rack p) ((==t) . fst) (getSquare b pos)

``` 

Monadic style allows us to remove all those case statments and write
`validateMove` in a far nicer style. In effect, the case statements
are all replaced by the one in the definition of the monad
instance. If any of the validation functions encounters an error, the
appropriate message is delivered.

```
validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Evaluator ()
validateMove b p w fm =
	   wordOnBoard w
	   >> connects w b fm 
	   >> straight w 
	   >> firstMoveTouchesCentre w fm 
	   >> lettersAvailable w p b
```
	
The validation functions are now *combinators*. We can combine small
ones like `connects` into larger ones like `validateMove` that check
several things. Functions at the top level can run an evaluator then
unpack the result in a single case statement to see if all went well
or, if not, exactly what went wrong.
				
## Checking words in the dictionary

We have already seen how to check that a word is in the dictionary
using `dictContainsWord`. Instead of returning a boolean we now want this to fit in
with the combinator style of validation, so we alter it to run in the `Evaluator`
monad.

```haskell
dictContainsWord :: Dict -> Text -> Evaluator ()
dictContainsWord d t = Trie.member t d `evalBool` ("Not in dictionary: " <> t) 
```

We need to apply this function to the new
word and all additional words generated by the move. The classic
instructions give the example of this board:

```
   F
   A
 HORN 
   M
 PASTE
```
In the next move the word MOB is played, generating two additional words,
NOT and BE. 

```
   F
   A
 HORN 
   MOB
 PASTE
```
Any bonus squares under the new letters (O and B) add to the score but no bonuses 
are counted for the letters that are already on the board. In the next move the 
word BIT is played, generating the additional words PI and AT.

```
   F
   A
 HORN 
   MOB
 PASTE
BIT
```
To find the additional words generated by a move we will first determine the 
direction of the new word. All additional words will be in the opposite direction.
Next we create a temporary board in which the word has been played and, for each position in the 
word, we see if that position is part of a word in the opposite direction. The `catMaybes`
function from `Data.Maybe` takes a list of `Maybe a` values and returns the list of `a` 
values from everything that isn't `Nothing`.

```haskell
import Data.Maybe (catMaybes)

additionalWords :: Board -> WordPut -> [WordPut]
additionalWords b w = 
  let b'     = updateBoard b w
      oppDir = if getDirection w == HZ then VT else HZ 
      mWds   = if oppDir == HZ then map (wordOnRow b') w else map (wordOnCol b') w in
   catMaybes mWds
```
To check that a word and all of its additional words are in the dictionary we first apply
`dictContainsWord` every word in the list. That function has this type:

```haskell
dictContainsWord :: Dict -> Text -> Evaluator ()
```
So if we `map` `dictContainsWord d` over a list of `Text` values we will get a list of
type `[Evaluator ()]`, a list of evaluations, whereas we want one evaluation with the list in 
it. The `mapM` function does what we need, having the type 

```haskell
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
```
If we use `mapM` to apply the function over the list of texts we get something with the type
`Evaluator [()]` instead of `[Evaluator ()]`, pulling the monad type out of the list. 

We then want to compress, or concatenate, the list of `()` values into
a single value so we can return the type `Evaluator ()`, since all we
want to know is that all the words are in the dictionary and if any of
them weren't we'd be getting back a `Left` with an error message in
it. The `mconcat` function works with all instances of the `Monoid`
typeclass and has the type `mconcat :: Monoid a => [a] -> a`. `()` is
a monoid so this function is the one we need.

```haskell
wordsInDictM :: Dict -> [Text] -> Evaluator ()
wordsInDictM t ws = mconcat <$> mapM (dictContainsWord t) ws
```

## Putting it all together

We have lots of ways in which we can validate moves and a nice neat way of combining them.
Validating that a move follows the most basic rules (e.g. the tiles are actually on the board)
is different from checking the words are in the dictionary, something we may or may not want to
do every time. It could be handy to turn off dictionary checking during development, and if an
AI player finds a word in the dictionary there's no point in checking it again.

We can turn the idea of validation into an abstraction (a type) and
combine the various smaller checks we have into coherent blocks.

```haskell
validateMove :: Board -> Player -> WordPut -> Bool -> Evaluator ()
validateMove b p w fm =
  wordOnBoard w
  >> connects w b fm
  >> straight w
  >> firstMoveTouchesCentre w fm
  >> lettersAvailable w p b

validateRack :: Board -> Rack -> WordPut -> Evaluator ()
validateRack b r w = someNewTiles b w >>
  all (\(pos,(t,_)) -> t `elem` r
           || (not (empty b pos) && (fst . fromJust . getSquare b) pos == t)) w
  `evalBool` ("Not all tiles in rack or on board: " ++ formatWP w)

type Validator = [WordPut] -> Game -> Evaluator ()

valGameRules :: Validator
valGameRules ws g = do
  let b  = board g
      p  = getPlayer g
      w  = head ws
      fm = firstMove g
  validateRack b (rack p) w >> validateMove b p w fm
  
valGameRulesAndDict :: Validator
valGameRulesAndDict ws g = do
  let d  = dict g
      ts = map (wordToText . map (fst .snd)) ws
  valGameRules ws g >> wordsInDictM d ts 

```

In the next chapter we will use these validators when we start playing moves
within the game.

## Tests

The tests from chapters one and two are refactored to work with the `Evaluator`
type. We add a series of tests relating to validating words in `Test.Chapter3`.
At this stage the tests start to look more complex. This is because when we
call functions in the `Evaluator` monad we have to unwrap the result by pattern
matching. Here is the test for the `wordOnBoard` validator.

```haskell
-- | Test the @wordOnBoard@ validation.
prop_wordOnBoard :: Property 
prop_wordOnBoard = monadicIO $ do
  gen <- liftIO getStdGen
  d   <- liftIO englishDictionary
  g   <- pick $ genGame gen d
  let wp = p1Word g
  case validateMove (g ^. board) (g ^. player1) wp True of
    Ev (Right _) -> assert True
    Ev (Left e)  -> do liftIO $ print e
                       assert False
  let e = makeWordPut (wordToText $ g ^. (player1 . rack)) (10,7) HZ []
  case validateMove (g ^. board) (g ^. player1) e False of
    Ev (Right _) -> assert False
    Ev (Left _)  -> assert True
```

## Exercises

+ Refactor the `straight` validator into two parts -- one called
  `straight` that checks the tiles are placed horizontally or vertically, and
  one called `continuous` that checks whether there are any gaps in the
  something goes wrong.
+ Change the tests so that if a `Left` value is returned you make sure the right
  error message is being received.
 
[Contents](../README.md) | [Chapter Four](Chapter4.md)

