
Explanation of Evaluator monad, with this example of nasty nested case statements:

```
-- | Play a word onto a board, updating the score of the current player
--   and resetting their rack. Returns the new game and the score of this move.
--   The word is validated as being in the dictionary.
move :: Dict    -- ^ The dictionary
     -> Game    -- ^ The game
     -> WordPut -- ^ The word to play
     -> Bool    -- ^ Is first move
     -> Either String (Game, Int)
move d g w fm =
  let b   = board g
      p   = getPlayer g
      aw  = additionalWords b w 
      sws = map (map (\(p',t') -> (p',t', empty b p'))) (w:aw) -- Only the new tiles should get bonuses
      waw = map (map snd) (w:aw)
      fpb = if newTilesInMove b w == 7 then 50 else 0
      sc  = sum $ map (scoreWord fpb) sws in 
  case validateMove b p w fm of
    Right _ -> case wordsInDict d waw of
      Right _ -> let g' = setScore g sc in
                 Right (g' {board = updateBoard b w}, sc)
      Left e -> Left e
    Left e -> Left e

{- Non-monadic version of validateMove for comparison
-- | Check that a move is valid: it touches at least one existing word (unless
--   it is the first move, in which case check that it touches the centre square),
--   it is in a straight and continuous line, and is made
--   up of letters that either in the rack or on the board.
validateMove :: Board   -- ^ The board
             -> Player  -- ^ The player making the move
             -> WordPut -- ^ The word to play
             -> Bool    -- ^ Is first move
             -> Either String Bool
validateMove b p w fm = case connects w b fm of
  Right _ -> case straight w of
               Right _ -> if all (\(pos,t) -> case getSquare b pos of
                                     Just l  -> l == t
                                     Nothing -> t `elem` rack p) w
                          then if fmGood
                               then Right True
                               else Left "First move must touch centre square"
                          else Left "Letters not in rack or not on board"
               Left e -> Left e
  Left e -> Left e
  where fmGood = not fm || touches (7,7) w 
-}

```
