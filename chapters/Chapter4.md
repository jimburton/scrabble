# Chapter Four: Playing against the computer

In this chapter a basic AI is added so games can be played against
the computer. In order to achieve this, a list of *playable* positions
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
	
![](/images/freedoms0.png)
	
Note that it would be possible to make a legal move by extending the
word with a prefix or suffix.  For instance, playing tiles to make the
word `FOULED`, or `BEFOUL` or even putting tiles before and after the
word to make `BEFOULED`. The AI currently makes no attempt to do
this. Nor does it try to get a high score! We will talk about these
optimisations at the end of the chapter
	
![](/images/freedoms1.png)
	
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
The function is called `findPrefixes` because it will include a word and all
of the prefixes of that word that are in the dictionary too, like PATSY, PATS
and PAT.

```haskell
-- | Convert a @Word@ to @Text@
wordToText :: Word -> Text
wordToText w = T.pack (wordToString w)

-- | Find all the prefixes in the dictionary that can be made with the given letters.
findPrefixes :: Game    -- ^ The game.
             -> Word    -- ^ The letters to build the words from.
             -> [Word]
findPrefixes g ls = findWords g (map wordToText (perms ls))

-- | Find all the words in the dictionary that can be made with the given letters.
findWords :: Game      -- ^ The dictionary to search
          -> [Text]    -- ^ The letters to build the words from.
          -> [Word]
findWords g ws = map textToWord $ filter (`Trie.member` dict g) ws
```

From the words returned by `findPrefixes`, we want only those that end
with the desired letter and are within the desired length. When we
come to look for suffixes of words we will also want to find only ones
of a certain length, and two functions that did that would be
identical apart from one or two details.  So we can factor this into a
function that finds words from the dictionary of a certain length, and
two functions that filter the dictionary search in different ways.

```haskell
-- A function for finding words in the dictionary.
type WordFinder = Word -> [Word] 
```

The `findPrefixOfSize` function makes a `WordFinder` that calls
`findPrefixes` then filters the output for those words that end with
the right letter. It passes this to `findWordOfSize`. It's possible
that there aren't any words that fit the bill, so these functions
returns a `Maybe` of the word it found and the additional words the
placement of this word will generate.

```haskell
-- Find a word of a certain size.
findWordOfSize :: Game             -- The game.
               -> WordFinder       -- Function that will query the dictionary.
               -> Pos              -- The start or end point of the word.
               -> Letter           -- The letter on the board that this word will connect to.
               -> Rack             -- The letters from the player's hand to make up the word.
               -> (FreedomDir,Int) -- The direction and max length of the word.
               -> Maybe (WordPut,[WordPut]) -- The word and its additional words.
findWordOfSize g wf k l r (fd,i) =
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
The `WordFinder`, `wf`, in the definition of `findWordOfSize` is doing the heavy lifting
of finding the words, whle the rest of the function is about picking the longest one,
making it into a `WordPut` and finding the additional words. 

The `findPrefixOfSize` function passes in a `WordFinder` that finds all prefixes then filters
then for ones that end with the desired letter. 

```haskell
-- Find a word of at least a certain size that ends with a certain letter.
findPrefixOfSize :: Game             -- The dictionary.
                 -> Pos              -- The end point of the word.
                 -> Letter           -- The letter on the board that this word will connect to.
                 -> Rack             -- The letters from the player's hand to make up the word
                 -> (FreedomDir,Int) -- The direction and max length of the word.
                 -> Maybe (WordPut, [WordPut]) -- The word and the additional words.
findPrefixOfSize g p l r (fd,i) =
  findWordOfSize g (filter ((==l) . last) . findPrefixes g) p l r (fd,i)
```

The `findSuffixOfSize` uses the feature of our trie that allos us to narrow
it down to only those words that begin with a certain sequence of letter. So 
we pass a game with a restricted dictionary to `findWordOfSize.

```haskell
-- Find a word of at least a certain size that begins with a certain letter.
findSuffixOfSize :: Game             -- The game.
                 -> Pos              -- The starting point of the word.
                 -> Letter           -- The letter on the board that this word will connect to.
                 -> Rack             -- The letters from the player's hand to make up the word
                 -> (FreedomDir,Int) -- The direction and max length of the word.
                 -> Maybe (WordPut,[WordPut]) -- The word and the additional words.
findSuffixOfSize g p l = let g' = g { dict = Trie.submap (letterToText l) (dict g)} in
  findWordOfSize g (findPrefixes g') p l
```

Finally, we can put all this together to find a workd.

```haskell
findWord :: Game     -- The game.
         -> Rack     -- The rack.
         -> Maybe (WordPut, [WordPut]) -- The word and the additional words.
findWord g r = msum $ Map.mapWithKey findWord' (playable g)
  where  findWord' k (l,ps) =
          msum $ map (\(fd,i) ->
                         case fd of
                           UpD    -> findPrefixOfSize g k l r (fd,i) 
                           DownD  -> findSuffixOfSize g k l r (fd,i) 
                           LeftD  -> findPrefixOfSize g k l r (fd,i) 
                           RightD -> findSuffixOfSize g k l r (fd,i)) ps
```
	
*Work in progress*: The AI would be much better if it were more
flexible about choosing where to play. At the moment it can only play
perpendicular to an existing word.  It could play words that by adding
letters to the beginning or end of existing ones, and could play words
with the playable position somewhere in the middle. It could also be
more careful about pruning playable positions.  In the example above
the position `(7,7)` which has the letter 'F' on it *is* playable, but
is currently removed from the list for simplicity. It could also put
up more of a fight by searching for the best move, but smarter
strategies would be needed to do this in reasonable time. These
strategies could include trying to make words using high value tiles
and which are placed on bonus tiles.
	
Because the module `Scrabble.Game.AI` needs to share a lot of code
with `Scrabble.Game.Game`, common code is moved into its own module,
`Scrabble.Game.Internal`. A similar change is made to the `Board`
code, adding `Scrabble.Board.Internal`.
	
Files in the library:
	
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

## In the REPL

```
> :m + Scrabble
> :m + System.Random
> theGen <- getStdGen
> d <- englishDictionary 
> let gAI = newGame1P "Ted" theGen d
> player1 gAI
Player
    { name = "Ted"
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
λ> player2 gAI
Player
    { name = "Haskell"
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
    , isAI = True
    }
> let w = [((7,7),(P,3)), ((7,8),(I,1)), ((7,9),(E,1))]
> let (Ev (Right (g1,(ws,sc)))) = move valGameRules gAI w []
> let (Ev (Right (g2,(wp,ws,bs,sc)))) = moveAI g1
> showBoard False (board g2)
"  | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|
------------------------------------------------
 0|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 1|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 2|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 3|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 4|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 5|  |  |  |  |  |  |  | Z|  |  |  |  |  |  |  |
 6|  |  |  |  |  |  |  | E|  |  |  |  |  |  |  |
 7|  |  |  |  |  |  |  | P| I| E|  |  |  |  |  |
 8|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
 9|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
10|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
11|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
12|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
13|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
14|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
------------------------------------------------
"
```
The Ai played the word ZEP (there are some very strange words
in the Scrabble dictionary). 

## Optimisation

This could be done by
modifying `Scrabble.Game.AI`, especially the `findWord` function and
the functions it depends on.

## Tests

TODO
 
[Contents](../README.md) | [Chapter Five](Chapter5.md)
