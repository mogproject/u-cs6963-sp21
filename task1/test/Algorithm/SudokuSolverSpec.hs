module Algorithm.SudokuSolverSpec (spec) where

import Algorithm.SudokuSolver
import Data.Either
import qualified Data.Set as Set
import Data.Sudoku
import Test.Hspec

parse :: String -> Sudoku
parse s = fromRight (emptySudoku 1 1) (readSudoku s)

spec :: Spec
spec = do
  describe "SudokuSolver#solveSudoku()" $ do
    it "finds no solution" $ do
      solveSudoku 0 (parse "1 2\n1 .\n. 2") `shouldBe` []

    it "finds all solutions" $ do
      Set.fromList (solveSudoku 0 (emptySudoku 1 2))
        `shouldBe` Set.fromList
          [ parse "1 2\n1 2\n2 1",
            parse "1 2\n2 1\n1 2"
          ]
