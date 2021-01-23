import Algorithm.SudokuSolver (solveSudoku)
import Control.Monad ((>=>))
import Data.Sudoku (Sudoku, readSudoku)

main :: IO ()
main = interact $ either (++ "\n") show . (readSudoku >=> solve)

solve :: Sudoku -> Either String Sudoku
solve sd = case solveSudoku sd of
  x : _ -> Right x
  _ -> Left "no solution"
