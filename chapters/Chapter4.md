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
maximum length of the word. The functions that search for words are in
`Scrabble.Lang.Search` and `Scrabble.Lang.Dict` and use the API of the
`Trie` datatype.
	
The freedoms map needs to be updated after each word is played -- each
new word *adds* new playable positions but also may *reduce* the
playable space around existing playable positions or remove playable
spaces entirely. The figure below shows the freedoms on the board
after the first move. The freedom of one of the positions with a tile
on it is shown: `[(LeftD, 7), (RightD, 7)]`. In this case, all of the
playable positions have the same freedom.
	
![](/images/freedoms0.png)
	
Note that it would be possible to make a legal move by extending the
word with a prefix or suffix.  For instance, playing tiles to make the
word `FOULED`, or `BEFOUL` or even putting tiles before and after the
word to make `BEFOULED`. The AI currently makes no attempt to do
this. Nor does it try to get a high score! This could be done by
modifying `Scrabble.Game.AI`, especially the `findWord` function and
the functions it depends on.
	
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
we want to calculate the row freedom (up and down), vice versa the column freedom
(left and right).

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

Now we need to map the `freedom` function over every position in a new `WordPut`.

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
chain of evaluations in the `move` function.

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

Now we now where the AI might be able to play a word, we need to pick
one based on the contents of the AI player's rack. To make things
easier for ourselves, we won't consider blanks for now.

If a freedom at position `p` is *up* or *to the left* we need to find
a word that *begins* with the tile on `p`. Otherwise we need to find a
word that *ends* with that tile. In each case, the freedom tells us the 
maximum size of the word. 



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

## Tests

TODO
 
[Contents](../README.md) | [Chapter Five](Chapter5.md)
