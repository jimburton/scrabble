# Chapter Four: Playing against the computer

An AI is added, so games can be played against the computer. In order
to achieve this, a list of *playable* positions is maintained. A
playable position is one where the AI could play a word, so we need to
know the letter at that position, the amount of space around it and
the direction of that space. Several new types are added to support
this.
	
A `FreedomDir` is a direction on the board -- `Upd`, `DownD`, `LeftD` or `RightD`.
	
A `Freedom` is a `FreedomDir` and a distance. 
	
Then we create a map with the type `Map Pos (Letter, [Freedom])`,
which is added to the game state and updated after each move is
played. When the AI comes to make a move it needs to repeatedly take a
playable position and try to create a word that can be played against
it. If the direction of the freedom is `UpD` or `LeftD` the AI needs
to find a word that *ends* with the letter in question. If the
direction is `RightD` or `DownD` it needs to find a word that *begins*
with the letter in question. In each case the freedom tells the AI the
maximum length of the word. The functions that search for words are in
`Scrabble.Lang.Search` and `Scrabble.Lang.Dict` and use the API of the
`Trie` datatype.
	
The freedoms map needs to be updated after each word is played -- each
new word adds new playable positions but also may reduce the playable
space around existing playable positions or remove them entirely. The
figure below shows the freedoms on the board after the first move. The
freedom of one of the positions with a tile on it is shown: `[(LeftD,
7), (RightD, 7)]`. In this case, all of the playable positions have
the same freedom.
	
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
below). This takes place in the function
`Scrabble.Game.Internal.updatePlayables`.
	
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
	
  + Because the module `Scrabble.Game.AI` needs to share a lot of code with `Scrabble.Game.Game`,
    common code is moved into its own module, `Scrabble.Game.Internal`. A similar change is made
	to the `Board` code, adding `Scrabble.Board.Internal`.
	
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
