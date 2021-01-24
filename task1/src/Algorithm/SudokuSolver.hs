module Algorithm.SudokuSolver (solveSudoku) where

import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sudoku (ExtendedPos, Pos, Sudoku, getAvailableNumbers, getEmptyPositions, getRegionId, insert)
import System.Random (Random (randomR), mkStdGen)

--------------------------------------------------------------------------------
-- Sudoku solver
--------------------------------------------------------------------------------

-- | Solves a Sudoku instance recursively.
solveSudoku :: Int -> Sudoku -> [Sudoku]
solveSudoku seed sd = solveSudoku' (Set.fromList (getEmptyPositions sd)) sd

solveSudoku' :: Set Pos -> Sudoku -> [Sudoku]
solveSudoku' ps sd =
  if null ps
    then do
      [sd] -- completed
    else do
      -- find a position with the least number of choices
      let positions = [(p, length xs, xs) | p <- Set.toList ps, let xs = getAvailableNumbers sd p]
      let minLength = minimum [len | (_, len, _) <- positions]

      if minLength == 0
        then do
          [] -- no solution
        else do
          let candidates = [(p, xs) | (p, len, xs) <- positions, len == minLength]
          let (p, xs) = head candidates
          xs >>= (maybeToList . insert sd p) >>= solveSudoku' (p `Set.delete` ps)
