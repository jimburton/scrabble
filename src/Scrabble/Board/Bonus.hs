module Scrabble.Board.Bonus
  ( bonusMap )
  where
import Prelude hiding ( Word )
import Data.Map ( Map )
import qualified Data.Map as Map
import Scrabble.Types
  ( Pos
  , Bonus(..) )

-- ========== Bonuses ============ --

-- | Data for the bonus map.
bonusSquaresList :: [((Int, Int), Bonus)] -- ^ ((Row, Column), Bonus)
bonusSquaresList =
  [((0, 0),   Word 3),    ((0, 3),   Letter 2)
  , ((0, 7),   Word 3),   ((0, 11),  Letter 2)
  , ((0, 14),  Word 3),   ((1, 1),   Word 2)
  , ((1, 5),   Letter 3), ((1, 9),   Letter 3)
  , ((1, 13),  Word 2),   ((2, 2),   Word 2)
  , ((2, 6),   Letter 2), ((2, 8),   Letter 2)
  , ((2, 12),  Word 2),   ((3, 0),   Letter 2)
  , ((3, 3),   Word 2),   ((3, 7),   Letter 2)
  , ((3, 11),  Word 2),   ((3, 14),  Letter 2)
  , ((4, 4),   Word 2),   ((4, 10),  Word 2)
  , ((5, 1),   Letter 3), ((5, 5),   Letter 3)
  , ((5, 9),   Letter 3), ((5, 13),  Letter 3)
  , ((6, 2),   Letter 2), ((6, 6),   Letter 2)
  , ((6, 8),   Letter 2), ((6, 12),  Letter 2)
  , ((7, 0),   Word 3),   ((7, 3),   Letter 2)
  , ((7, 7),   Word 2),   ((7, 11),  Letter 2)
  , ((7, 14),  Word 3),   ((8, 2),   Letter 2)
  , ((8, 6),   Letter 2), ((8, 8),   Letter 2)
  , ((8, 12),  Letter 2), ((9, 1),   Letter 3)
  , ((9, 5),   Letter 3), ((9, 9),   Letter 3)
  , ((9, 13),  Letter 3), ((10, 4),  Word 2)
  , ((10, 10), Word 2),   ((11, 0),  Letter 2)
  , ((11, 3),  Word 2),   ((11, 7),  Letter 2)
  , ((11, 11), Word 2),   ((11, 14), Letter 2)
  , ((12, 2),  Word 2),   ((12, 6),  Letter 2)
  , ((12, 8),  Letter 2), ((12, 12), Word 2)
  , ((13, 1),  Word 2),   ((13, 5),  Letter 3)
  , ((13, 9),  Letter 3), ((13, 13), Word 2)
  , ((14, 0),  Word 3),   ((14, 3),  Letter 2)
  , ((14, 7),  Word 3),   ((14, 11), Letter 2)
  , ((14, 14), Word 3)]

-- | Map containing the bonus squares on the board.
bonusMap :: Map Pos Bonus
bonusMap = Map.fromList bonusSquaresList
