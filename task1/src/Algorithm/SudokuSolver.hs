module Algorithm.SudokuSolver (solveSudoku) where

import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sudoku (Pos, Sudoku, getAvailableNumbers, getEmptyPositions, insert)
import System.Random (StdGen, mkStdGen, randomR)

--------------------------------------------------------------------------------
-- Sudoku solver
--------------------------------------------------------------------------------

-- | Solves a Sudoku instance recursively.
solveSudoku :: Int -> Sudoku -> [Sudoku]
solveSudoku seed sd = solveSudoku' (mkStdGen seed) (Set.fromList (getEmptyPositions sd)) sd

solveSudoku' :: StdGen -> Set Pos -> Sudoku -> [Sudoku]
solveSudoku'
  rnd  -- pseudo random number generator
  ps -- set of empty grids
  sd -- partially filled Sudoku instance
  =
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

          -- tie breaking by a pseudo random number
          let (i, gen) = randomR (0, length candidates - 1) rnd
          let (p, xs) = candidates !! i

          xs >>= (maybeToList . insert sd p) >>= solveSudoku' gen (p `Set.delete` ps)
