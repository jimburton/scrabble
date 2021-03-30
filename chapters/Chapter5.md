# Chapter Five: Playing against the computer

[Contents](../README.md)

In this chapter a basic AI is added so games can be played against
the computer. 

Most of the code in this chapter will go into a new module
`Scrabble.Game.AI`.  This needs to share a lot of code with
`Scrabble.Game.Game`, so common code is moved into its own module,
`Scrabble.Game.Internal`. Similar to the benefits of having seperate 
`Types` module, this helps with avoiding circular imports and is a 
pretty common practice in Haskell development. To make it easier for
clients to find functions, `Scrabble.Game.Game` will re-export the 
important functions from `Scrabble.Game.Internal`.

A similar change is made to the `Board` code, adding `Scrabble.Board.Internal`.
This is the last major refactoring we'll need to do to the library.
	
The final state of files in the library:
	
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

To accomodate the AI player, a list of *playable* positions
is maintained. A playable position is one where the AI could play a
word, so we need to know the letter at that position, the amount of
space around it and the direction of that space. Several new types are
added to support this. We call that playable space a *freedom*.

## Calculating freedom
	
A `FreedomDir` is a direction on the board. A `Freedom` is a `FreedomDir` 
and a distance.

```haskell
data FreedomDir = UpD     -- ^ The Up direction.
                | DownD   -- ^ The Down direction.
                | LeftD   -- ^ The Left direction.
                | RightD  -- ^ The Right direction.
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

-- | A Freedom is a direction and a distance.
type Freedom = (FreedomDir, Int)
```
	
Then we can create a map with the type `Map Pos (Letter, [Freedom])`,
which is added to the game state and updated after each move is
played.

When the AI comes to make a move it needs to repeatedly take a
playable position and try to create a word that can be played against
it. If the direction of the freedom is `UpD` or `LeftD` the AI needs
to find a word that *ends* with the letter in question. If the
direction is `RightD` or `DownD` it needs to find a word that *begins*
with the letter in question. In each case the freedom tells the AI the
maximum length of the word.
	
The freedoms map needs to be updated after each word is played -- each
new word adds new playable positions but also may reduce the
playable space around existing playable positions, or remove playable
spaces entirely. The figure below shows the freedoms on the board
after the first move. The freedom of one of the positions with a tile
on it is shown: `[(LeftD, 7), (RightD, 7)]`. In this case, all of the
playable positions have the same freedom.
	
<img src="/chapters/images/freedoms0.png" alt="Scrabble board" width="500px" />
	
Note that it would be possible to make a legal move by extending the
word with a prefix or suffix.  For instance, playing tiles to make the
word `FOULED`, or `BEFOUL` or even putting tiles before and after the
word to make `BEFOULED`. The AI currently makes no attempt to do
this. Nor does it try to make sure it gets the highest possible score. As
we will see, at the moment it just tries to play the longest word. We
will talk about ways to improve this at the end of the chapter.
	
<img src="/chapters/images/freedoms1.png" alt="Scrabble board" width="500px" />
	
The figure above shows what happens after more tiles are placed on the
board. Several freedoms have been removed (too many, in fact -- see
below). 

For each new `WordPut`, `wp`, we have to calculate the freedoms from 
each position in `wp`. For any given position we can calculate the
free space above and below it, or to the right and left of it, with
these functions:

```haskell
-- Is this position on the board and unoccupied?
canPlay :: Board -> Pos -> Bool
canPlay b p = onBoard p && isNothing (getSquare b p)

-- The playable space above and below this position.
rowFreedom :: Board        -- ^ The board.
           -> Pos          -- ^ The pos.
           -> Letter       -- ^ The letter on the pos.
           -> (Pos, Letter, (Freedom, Freedom))
rowFreedom b (r,c) l =
  let mins = takeWhile (\p -> canPlay b p && (fst p == 0 || canPlay b (decRow p)))
             (iterate decRow (r,c))
      minR = if null mins then r else fst (last mins)  
      maxs = takeWhile (\p -> canPlay b p && (fst p == 14 || canPlay b (incRow p)))
             (iterate incRow (r,c))
      maxR = if null maxs then r else fst (last maxs)  in
    ((r,c),l, ((UpD, r-minR), (DownD, maxR-r)))

-- The playable space to the left and right of this position.
colFreedom :: Board        -- ^ The board.
           -> Pos          -- ^ The pos.
           -> Letter       -- ^ The letter on the pos.
           -> (Pos, Letter, (Freedom, Freedom))
colFreedom b (r,c) l =
  let mins = takeWhile (\p -> canPlay b p && (fst p == 0 || canPlay b (decCol p)))
             (iterate decCol (r,c))
      minC = if null mins then c else snd (last mins)  
      maxs = takeWhile (\p -> canPlay b p && (fst p == 14 || canPlay b (incCol p)))
             (iterate incCol (r,c))
      maxC = if null maxs then c else snd (last maxs) in 
    ((r,c),l, ((LeftD, c-minC), (RightD, maxC-c)))
```

When a move is played, we only want to calculate the freedoms in one direction, the
opposite of the one in which the word was played. If a word was played horizontally
we want to calculate the row freedom (up and down), and if *vice versa* the column freedom
(to the left and right).

```haskell
-- The playable space around an occupied position on the board.
freedom :: Board  -- ^ The board.
        -> Pos    -- ^ The pos.
        -> Letter -- ^ The letter on the pos.
        -> Dir    -- ^ The direction of the word the letter is part of.
        -> (Pos, Letter, (Freedom, Freedom))
freedom b p l d =
  if d == HZ
  then rowFreedom b p l 
  else colFreedom b p l 
```

Now we can map the `freedom` function over every position in a new `WordPut`.

```haskell
-- | The value of a Freedom
freeness :: Freedom -> Int
freeness = snd

-- | All of the playable spaces around a word on the board.
freedomsFromWord :: WordPut -> Board -> [(Pos, Letter, (Freedom, Freedom))]
freedomsFromWord w b =
  let d  = getDirection w in 
    filter (\(_,_,(n,s)) -> freeness n > 0 || freeness s > 0) 
    $ map (\(p,(l,_)) -> freedom b p l d) w
```

## Updating the map of playable positions

We write a function `updatePlayables`, which needs to be added to the
chain of evaluations after `updateBoard` in the `move` function.

```haskell
newTiles :: Board -> WordPut -> [(Pos, Tile)]
newTiles b = filter (\(p,_) -> isNothing (getSquare b p))

adjacent :: Pos -> Pos -> Bool
adjacent (r1,c1) (r2,c2) = abs (r1-r2) <= 1 && abs (c1-c2) <= 1

updatePlayables :: WordPut -> Game -> Evaluator Game
updatePlayables w g = do
  let ps  = playable g
      b   = board g
      nt  = newTiles b w
      ntp = map fst nt
      -- assuming any existing playable that is adjacent to the new word
      -- will no longer be playable.
      ps' = Map.filterWithKey (\k _ -> not (any (adjacent k) ntp))  ps
      fs  = freedomsFromWord nt b
      f (u,p) = filter ((>0) . freeness) [u, p]
      nps = foldl (\acc (p,l,(n,s)) -> Map.insert p (l,f (n,s)) acc) ps' fs
  pure (g { playable = nps })
```

## Finding a word

Now we know where the AI might be able to play a word, we need to pick
a word based on the contents of the AI player's rack. To make things
easier for ourselves, we won't consider blanks for now.

We'll start by finding all the words that can be made with a rack and
that end with a given letter that isn't in the rack. We write
functions to find all possible combinations of a rack. We start by
finding the *powerset* of the letters, the set (or list, in this case)
of all subsets (sublists). The function that does that, `powerSet`, is
very concise! It makes use of the fact that the list type is a monad. 
Read more about it at http://evan-tech.livejournal.com/220036.html, or
just accept the fact that it works :-)

```haskell
-- Generate a power set.  The algorithm used here is from
--   <http://evan-tech.livejournal.com/220036.html>.
powerSet :: [a] -> [[a]]
powerSet = filterM (const domain)
```
Then we use the `permutations` function from `Data.List`, which
generates all combinations of the contents of a list, and remove
any duplicates from the list.

```haskell
-- Generate a power set's permutations.
powerSetPermutations :: [a] -> [[a]]
powerSetPermutations = concatMap permutations . powerSet

-- Generate a power set's unique permutations.
uniquePowerSetPermutations :: Eq a => [a] -> [[a]]
uniquePowerSetPermutations = nub . powerSetPermutations
```
Finally, words need to be at least two letters long so in the
`perms` function we use `filter`.

```haskell
-- Permutations of a list
perms :: Eq a => [a] -> [[a]]
perms = filter ((>1) . length) . uniquePowerSetPermutations 
```
Now we can find out which of these permutations are words in the dictionary.
We will need to consider two cases -- words made from a permutation with a
given letter appended to the end, and words made from a permutation with a letter
consed on the front.

```haskell
-- | Convert a @Word@ to @Text@
wordToText :: Word -> Text
wordToText w = T.pack (wordToString w)

-- | Find all the prefixes in the dictionary that end with the given letter.
findPrefixesForLetterL :: Game    -- ^ The game.
              -> Letter -- ^ The suffix.
              -> Word   -- ^ The letters to build the words from.
              -> [Word]
findPrefixesForLetterL g l ls = findWords g (map (wordToText . (++[l])) (perms ls))

-- | Find all the prefixes in the dictionary that begin with the given letter.
findSuffixesForLetter :: Game    -- ^ The game.
                      -> Letter -- ^ The suffix.
                      -> Word   -- ^ The letters to build the words from.
                      -> [Word]
findSuffixesForLetter g l ls = findWords g (map (wordToText . (l:)) (perms ls))

-- | Find all the words in the dictionary that can be made with the given letters.
findWords :: Game      -- ^ The dictionary to search
          -> [Text]    -- ^ The letters to build the words from.
          -> [Word]
findWords g ws = map textToWord $ filter (`Trie.member` dict g) ws
```
Now we need to write functions that find words of a given size and which either
begin with or end with a certain letter. Two functions that do that would be identical 
apart from small details.  So we can factor this into a function that finds words from
the dictionary of a certain length, and then two functions that filter
the results of that search in different ways.

```haskell
-- A function for finding words in the dictionary.
type WordFinder = Word -> [Word] 

-- Find a word of a certain size.
findWordOfSize :: Game             -- The game.
               -> WordFinder       -- Function that will query the dictionary.
               -> Pos              -- The start or end point of the word.
               -> Rack             -- The letters from the player's hand to make up the word.
               -> (FreedomDir,Int) -- The direction and max length of the word.
               -> Maybe (WordPut,[WordPut]) -- The word and its additional words.
findWordOfSize g wf k r (fd,i) =
  let r' = l : take 5 (filter (/=Blank) r)
      ws = filter ((<=i) . length) $ wf r' in
    if null ws
    then Nothing
    else let d   = dict g
             w   = longest ws
             len = length w - 1
             dir = if fd == UpD || fd == DownD then VT else HZ
			 -- where does this word begin?
             pos = case fd of
               UpD    -> (fst k-len,snd k)
               DownD  -> k
               LeftD  -> (fst k,snd k-len)
               RightD -> k
             wp = makeWordPut (wordToString w) pos dir [] in
           case additionalWords g wp of
             Ev (Left _)   -> Nothing
             Ev (Right aw) -> Just (wp,aw)
```
The `WordFinder`, `wf`, in `findWordOfSize` will be doing the heavy lifting
of finding the words, whle the rest of the function is about picking the longest one,
making it into a `WordPut` and finding the additional words.


```haskell
-- Find a word of at least a certain size that ends with a certain letter.
findPrefixOfSize :: Game             -- The dictionary.
                 -> Pos              -- The end point of the word.
                 -> Letter           -- The letter on the board that this word will connect to.
                 -> Rack             -- The letters from the player's hand to make up the word
                 -> (FreedomDir,Int) -- The direction and max length of the word.
                 -> Maybe (WordPut, [WordPut]) -- The word and the additional words.
findPrefixOfSize g p l = findWordOfSize g (findPrefixesForLetter g l) p

-- Find a word of at least a certain size that begins with a certain letter.
findSuffixOfSize :: Game             -- The game.
                 -> Pos              -- The starting point of the word.
                 -> Letter           -- The letter on the board that this word will connect to.
                 -> Rack             -- The letters from the player's hand to make up the word
                 -> (FreedomDir,Int) -- The direction and max length of the word.
                 -> Maybe (WordPut,[WordPut]) -- The word and the additional words.
findSuffixOfSize g p l = findWordOfSize g (findSuffixesForLetter g l) p
```
Finally, we can put all this together to find a word. Inside the `findWord` function
we map an inner function, `findWord'`, over the map of playable positions. This creates
a list of `Maybes`, each of which is `Nothing` or the longest word playable at a position.
We filter the `Nothing` values and find the longest word in the list, if there is one. 

There is a lot happening in the `findWord` function. Try to read and understand its
constituent parts -- the top level of `findWord`, the inner function `findWord'` and
the helper function `maxWd` -- one at a time.

```haskell
-- Pick a word for the AI to play, along with the additional words it generates. 
findWord :: Game     -- The game.
         -> Rack     -- The rack.
         -> Maybe (WordPut, [WordPut]) -- The word and the additional words.
findWord g r =
  let ws = Map.foldlWithKey (\acc k v -> case findWord' k v of
                                Nothing  -> acc
                                Just mws -> mws : acc) [] (playable g) in
    maxWd ws
  where findWord' :: Pos -> (Letter,[Freedom]) -> Maybe (WordPut, [WordPut])
        findWord' k (l,fs) =
          let mwds = map (\(fd,i) ->
                             case fd of
                               UpD    -> findPrefixOfSize g k l r (fd,i) 
                               DownD  -> findSuffixOfSize g k l r (fd,i) 
                               LeftD  -> findPrefixOfSize g k l r (fd,i) 
                               RightD -> findSuffixOfSize g k l r (fd,i)) fs
              wds = catMaybes mwds in
            maxWd wds
        maxWd :: [(WordPut, [WordPut])] -> Maybe (WordPut,[WordPut])
        maxWd wds = if null wds
                    then Nothing
                    else Just (maximumBy (\o1 o2 -> length (fst o1)
                                           `compare` length (fst o2)) wds)
```

Now that we can find a word, we can play a move. In thinking about
writing the `moveAI` function we uncover a discrepancy with the way
`move` works for human players. That function returns a pair with type
`(Game, ([Word],Int))` in the `Evaluator` monad, where the list of
words is the word played and all additional words and the int is the
score. This won't quite do for the AI version. We need to return the
word to play as a `WordPut`, so we know where to put it. Although we
haven't handled blanks yet, when we do we will need to know which positions
in the word were originally blank, so we will have to return a list of indices
too. Taking the updated game and the score into account, this is an awful lot 
to pack into a tuple so for readability we'll make a Record type, `MoveResult`, and
refactor the `move` function to return the same type.

```haskell
data MoveResult = MoveResult { mvWord :: WordPut
	                         , mvAdditionalWords :: [Word]
                             , mvBlanks          :: [Int]
                             , mvScore           :: Int
                             }
                             deriving (Show)
				 
-- | Play a word onto a board as the AI player, Returns the new game and the score of this move.
--   Validation of the word is carried out when finding the word.
--   Sets the new board, updates the current player's score, refills their rack with letters, then
--   toggles the current turn.
--   Returns:
--     + the updated game,
--     + the word played,
--     + additional words generated by the move,
--     + the indices of any blanks played in the move, and
--     + the score.
moveAI :: Game      -- ^ The game.
       -> Evaluator (Game, Move)
moveAI g = do
  let r  = rack (getPlayer g)
      mw = findWord g (filter (/=Blank) r)
  case mw of
    Nothing -> pass g >>= \g' -> pure (g',([],[],[],0))
    Just (w,aw)  -> scoreWords g w aw >>=
                    \i -> setScore g { firstMove = False } i >>= updatePlayer w
                    >>= updatePlayables w >>= updateBoard w
                    >>= toggleTurn <&> (,Move w (map wordPutToWord (w:aw)) [] i)
```

Finally for the code in this chapter, we need to begin a new AI
game. This is very similar to the previous `newGame` function but the
second player is generated by the library.


```haskell
-- | Start a new game against the computer.
newGame1P :: Text   -- ^ Name of Player
          -> StdGen   -- ^ The random generator
          -> Dict -- ^ The dictionary
          -> Game
newGame1P pName theGen d = 
  let Ev (Right (rack1, bag1, gen')) = fillRack [] newBag theGen
      p1 = Player { name  = pName
                  , rack  = rack1
                  , score = 0
                  , isAI  = False }
      Ev (Right (rack2, bag2, gen'')) = fillRack [] bag1 gen'
      p2 = Player { name  = "Haskell"
                  , rack  = rack2
                  , score = 0
                  , isAI  = True }
      g  = Game { board     = newBoard
                , bag       = bag2
                , player1   = p1
                , player2   = p2
                , turn      = P1
                , gen       = gen''
                , firstMove = True
                , dict      = d
                , gameOver  = False
                , playable  = Map.empty
                , lastMovePass = False } in
    g
```
	
## Work in progress

The AI would be much more effective if it were more flexible about
choosing where to play. At the moment it can only play perpendicular
to an existing word.  It could play words that by adding letters to
the beginning or end of existing ones, and could play words with the
playable position somewhere in the middle. It could also be more
careful about pruning playable positions.  In the example above the
position `(7,7)` which has the letter 'F' on it *is* playable, but is
currently removed from the list for simplicity. It could also put up
more of a fight by searching for the best move, but smarter strategies
would be needed to do this in reasonable time. These strategies could
include trying to make words using high value tiles and which are
placed on bonus tiles.

## In the REPL

Let's have a go at playing against the computer.

```
> :m + Scrabble
> :m + System.Random
> g <- getStdGen
> d <- englishDictionary 
> let g1 = newGame1P "Bob" g d
```

Let's have a look at the players:

```
> player1 g1
Player
    { name = "Bob"
    , rack =
        [ L
        , N
        , A
        , J
        , Blank
        , N
        , E
        ]
    , score = 0
    , isAI = False
    }
> player2 g1
Player
    { name = "Haskell"
    , rack =
        [ L
        , P
        , E
        , H
        , E
        , R
        , V
        ]
    , score = 0
    , isAI = True
    }
```

Now we'll take a move with Bob's rack and let the AI have a go.

```
> let w = [((7,7),(L,1)), ((8,7),(A,1)), ((9,7),(N,1)), ((10,7),(E,1))]
> let (Ev (Right (g2,mv))) = move valGameRules g1 w []
> let (Ev (Right (g3,mv))) = moveAI g2 
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
 7|  |  |  |  |  |  |  | L|  |  |  |  |  |  |  |
 8|  |  |  |  |  |  |  | A| L| E| P| H|  |  |  |
 9|  |  |  |  |  |  |  | N|  |  |  |  |  |  |  |
10|  |  |  |  |  |  |  | E|  |  |  |  |  |  |  |
11|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
------------------------------------------------
"
```
The Ai played the word ALEPH. At least that's a beginning :-) 

## Tests

TODO
 
[Contents](../README.md) | [Chapter Six](Chapter6.md)
