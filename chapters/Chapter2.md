# Validating moves

The code corresponding to this version of the project is in the branch `chapter2`.

In this chapter we develop an error checking style that will serve as 
a good design for the entire application. It will allow us to thread
the state of the game through a long and growing series of computations
without having to worry at each stage about whether anything went wrong.

## Checking the structure and position of words

We can place words onto the board, so now we want to check whether
doing so is valid. The rules about word placement are as follows:

+ Words are at least two letters long.
+ Words must fit on the board.
+ A word must be continuous and either horizontal or vertical.
+ The first word must cross the centre square, while subsequent words
  must connect with an existing word.
+ The word and all additional words that it generates must be in the
  dictionary.
  
Two positions on the board, `(r1,c1)` and `(r2,c2)`, are in a coninuous straight
line if either `r1==r2-1` and `c1==c2` or `r1==r2` and `c1==c2-1`. One approach
would be as follows:

```haskell
straight :: WordPut -> Bool
straight (w:x:xs) = 
  let ws      = map fst (w:x:xs)
      (r,_)   = fst w
      (r',_)  = fst x
      (s1,s2) = if r == r'-1 then (fst,snd) else (snd,fst)
      f       = \(x',y') -> s1 x' == s1 y' - 1 && s2 x' == s2 y' in 
    (all f . zip ws $ tail ws)
straight _  = error "Too few letters"
```

But we don't want to throw a runtime error that crashes the program if
words are less than two letters long. Equally, if the word isn't straight
, we'd like to return a descriptive error message. As there are lots of ways in
which a move might be invalid we'd like to be able to let the user know
exactly what went wrong. 

A common solution to writing a function that returns a value of type
`a` or an error message is to use the type `Either Text a`. Values of
this type are either `Left e`, where `e` is an error message, or
`Right x` where `x :: a`. In the case of `straight`, if it returns any
type of `Right` value then we know things went well. So there's no
need to return a `Bool`, we can just return `Left e` for an error or
`Right ()` if things went well. `()` ("unit") is the type with exactly
one value in it (which is also `()`):

```haskell
straight :: WordPut -> Either String ()
straight (w:x:xs) = 
  let ws      = map fst (w:x:xs)
      (r,_)   = fst w
      (r',_)  = fst x
      (s1,s2) = if r == r'-1 then (fst,snd) else (snd,fst)
      f       = \(x',y') -> s1 x' == s1 y' - 1 && s2 x' == s2 y' in 
    if (all f . zip ws $ tail ws)
	then Right ()
	else Left "Word not in a straight line" 
straight _ = Left "Too few letters"
```

To check whether a word connects with an existing word, we need to look at the
"neighbours" of each position in the new word and check whether at least one
neighbour has a tile in it. Since this only applies if this is not the first move,
we pass a boolean parameter which is true if this is the first move.

```haskell
onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r < 15 && c >= 0 && c < 15

neighbours :: Pos -> [Pos]
neighbours (r,c) = filter onBoard [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

getSquare :: Board -> Pos -> Maybe Tile
getSquare b pos = if onBoard pos
                    then b ! pos
                    else Nothing

occupiedNeighbours :: Board -> Pos -> [Pos]
occupiedNeighbours b pos = filter (isJust . getSquare b) $ neighbours pos

connects :: WordPut -> Board -> Bool -> Either Text ()
connects [] _ fm     = if fm then Right () else Left "Not touching any other tile"
connects (w:ws) b fm = let (pos,_) = w in
  if (not . all null) (occupiedNeighbours b pos)
  then Right ()
  else connects ws b fm
```
Checking that a word is on the board just means checking that every position is on
the board.

```haskell
wordOnBoard :: WordPut -> Either String ()
wordOnBoard w = if all (onBoard . fst) w
                 then Right ()
				 else Left "Word not on board"
```
Checking that the first word touches the centre square is also pretty
straightforward with the `touches` function.

```haskell
touches :: Pos -> WordPut -> Bool
touches p = any ((==p) .  fst)

firstMoveTouchesCentre :: WordPut -> Bool -> Either Text ()
firstMoveTouchesCentre w fm = if (not fm || touches (7,7) w) 
                               then Right ()
							   else Left "First move must touch the centre square"
```
We also need to check that the letters in the word to be played are actually in the player's 
rack or are already on the board.

```haskell
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
function calls will return an `Either String a` and we may find
ourselves doing a lot of case statements and pattern matching on
`Either` values. A function that puts together the various ways we
migh validate move could look like this:
	
```
validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
validateMove b p w fm = case connects w b fm of
  Right _ -> case straight w of
               Right _ -> case firstMoveTouchesCentre w fm of
                            Right _ -> case lettersAvailable w p b of
                                         Right -> Right ()
                                         Left e -> Left e
                            Left e -> Left e
               Left e -> Left e
  Left e -> Left e
 ```
 
The technical term for this kind of code is "filthy". Such deeply
nested and indented code as this is very hard to read, hard to
maintain and hard to extend.  Fortunately, what we can do here is to
use a monad to encapsulate the checks for `Left` and `Right`. We make
our `Either` type into a monad, where the monad instance says what to
do when we encounter a `Left` value, and then when we use the
monad we can carry on as if everything is a `Right` value -- no more
case statements.

We create a new type for arbitrary "evaluations" in the game, called
`Evaluator`. 

```haskell
newtype Evaluator a = Ev (Either Text a)
```
This type wraps up an `Either Text a` type where, as you
may expect, the `Text` is an error message and the `a` value is
whatever is being evaluated. For instance, `a` may be an updated
version of the game, or just `()` in cases where moves are being
checked for validity. Now we need to make a monad instance for the type.
That requires us to define the `Applicative` and `Functor` instances for
`Evaluator`, since every monad is an applicative and every applicative 
is a functor. The spirit of these definitions is that if we are dealing
with an `Ev (Left _)` value we want to **stop what we are doing and report 
the error**, while if we are dealing with a `Ev (Right _)` value we can
**keep going**.

```haskell
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
evalBool :: Bool -> String -> Evaluator ()
evalBool b e = if b then pure () else fail e
```
Now our validation functions will all have a similar structure to the new version 
of `lettersAvailable` below -- a call to `evalBool` where the first argument is a boolean 
condition and the second is an error message.

```haskell
lettersAvailable :: WordPut -> Player -> Board -> Evaluator ()
lettersAvailable w p b = all available w `evalBool`"Letters not in rack or not on board."
  where available (pos,(t,_)) = maybe (t `elem` rack p) ((==t) . fst) (getSquare b pos)

```
Monadic style allows us to remove all those case statments and write `validateMove` in
a far nicer style:

```
validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Evaluator ()
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
computations to see if all went well. 	```
								 
## Checking words in the dictionary

We have already seen how to check that a word is in the dictionary
using `dictContainsWord`. Instead of returning a boolean we now want this to fit in
with the combinator style of validation, so we alter it to run in the `Evaluator`
monad.
```haskell
dictContainsWord :: Dict -> Text -> Evaluator ()
dictContainsWord d t = Trie.member t d `evalBool` ("Not in dictionary: " ++ show t) 
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
The new letters (O and B) benefit from any bonus squares
they are played on but no bonuses are counted for the letters that are 
already on the board. In the next move the word BIT is played, generating 
the additional words PI and AT.
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

```haskell
data Dir = HZ | VT deriving (Show, Read, Eq)

getDirection :: WordPut -> Dir
getDirection w = let r1 = fst $ fst $ head w
                     r2 = fst $ fst $ head (tail w) in
                   if  r1<r2 then VT else HZ
```
Now we create the board in which the word has been played and for each position in the 
word we see if that position is part of a word in the opposite direction. This is what the
functions `wordOnRow`, `wordOnCol`, `startOfWord` and `wordFromPos` do. We also define 
some functions that transform positions and a convenient type for them, `PosTransform`. 
The `catMaybes`function from `Data.Maybe` takes a list of `Maybe a` values and returns 
the list of `a` values from everything that isn't `Nothing`.

```haskell
import Data.Maybe (catMaybes)

type PosTransform = Pos -> Pos

incRow :: PosTransform
incRow (r,c) = (r+1,c)

incCol :: PosTransform
incCol (r,c) = (r,c+1)

startOfWord :: Board -> PosTransform -> Pos -> Pos
startOfWord b f pos = let pos' = f pos in
  if not (onBoard pos') || isNothing (getSquare b pos')
  then pos
  else startOfWord b f pos'

wordFromSquare :: Board -> PosTransform -> Pos -> Maybe WordPut
wordFromSquare b f pos =  maybe [] (\t -> (pos, t) 
                                     : wordFromSquare b f (f pos)) (getSquare b pos)

wordOnRow :: Board -> Pos -> Maybe WordPut
wordOnRow b pos = wordFromSquare b incCol (startOfWord b decCol pos)

wordOnCol :: Board -> Pos -> Maybe WordPut
wordOnCol b pos = wordFromSquare b incRow (startOfWord b decRow pos)

additionalWords :: Board -> WordPut -> [WordPut]
additionalWords b w = 
  let b'     = updateBoard b w
      oppDir = if getDirection w == HZ then VT else HZ 
      mWds   = if oppDir == HZ then map (wordOnRow b') w else map (wordOnCol b') w in
   catMaybes mWds
```
To check that a word and all of its additional words are in the dictionary we apply
`dictContainsWord` every word in the list then concatenate the result. Thanks to monadic 
style, we never need to worry about whether a word isn't found or which one it might be --
any error will bubble up and be handled in the right way.

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

Lets make a type for validation and combine the various smaller checks we have into coherent
blocks.

```haskell
validateMove :: Board -> Player -> WordPut -> Bool -> Evaluator ()
validateMove b p w fm =
  connects w b fm
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

[Contents](../README.md) | [Chapter Three](Chapter3.md)

