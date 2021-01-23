module Algorithm.SudokuSolver (solveSudoku) where

import Data.Maybe (maybeToList)
import Data.Sudoku (Sudoku, getAvailableNumbers, getEmptyPositions, insert)

--------------------------------------------------------------------------------
-- Sudoku solver
--------------------------------------------------------------------------------

-- | Solves a Sudoku instance recursively.
solveSudoku :: Sudoku -> [Sudoku]
solveSudoku sd =
  case getEmptyPositions sd of
    pos : _ -> getAvailableNumbers sd pos >>= (maybeToList . insert sd pos) >>= solveSudoku
    _ -> [sd] -- completed
