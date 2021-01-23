module Data.SudokuSpec (spec) where

import Data.Sudoku
import Test.Hspec

spec :: Spec
spec = do
  describe "Sudoku#readSudoku()" $ do
    it "fails when input is invalid" $ do
      readSudoku "" `shouldBe` Left "missing lines"
      readSudoku "1" `shouldBe` Left "n and m must be specified in the first line"
      readSudoku "a" `shouldBe` Left "parse error: a"
      readSudoku "[]" `shouldBe` Left "parse error: []"
      readSudoku "a b" `shouldBe` Left "parse error: a b"
      readSudoku "a b c" `shouldBe` Left "parse error: a b c"
      readSudoku "1 2" `shouldBe` Left "invalid number of rows"
      readSudoku "1.0 2" `shouldBe` Left "parse error: 1.0 2"
      readSudoku "100000000000000000000" `shouldBe` Left "n and m must be specified in the first line"
      readSudoku "1 2 3" `shouldBe` Left "n and m must be specified in the first line"
      readSudoku "1 1 1\n." `shouldBe` Left "n and m must be specified in the first line"
      readSudoku "1 2\n.\n." `shouldBe` Left "invalid number of columns"
      readSudoku "1 0\n." `shouldBe` Left "n and m must be positive integers"
      readSudoku "0 1\n." `shouldBe` Left "n and m must be positive integers"
      readSudoku "0 0\n." `shouldBe` Left "n and m must be positive integers"
      readSudoku "1 -1\n." `shouldBe` Left "n and m must be positive integers"
      readSudoku "2 2\n1 1 . .\n. . . .\n. . . .\n. . ." `shouldBe` Left "invalid number of columns"
      readSudoku "2 2\n1 1 . .\n. . . .\n. . .\n. . . ." `shouldBe` Left "invalid number of columns"
      readSudoku "2 2\n1 1 . .\n. . . .\n. . . .\n. . . ." `shouldBe` Left "invalid board"
      readSudoku "2 2\n1 2 . 1\n. . . .\n. . . .\n. . . ." `shouldBe` Left "invalid board"
      readSudoku "2 2\n1 2 . .\n. . . .\n. 2 . .\n. . . ." `shouldBe` Left "invalid board"
      readSudoku "2 2\n1 2 . .\n. . . .\n. . . 3\n. . 3 ." `shouldBe` Left "invalid board"
      readSudoku "2 2\n1 5 . .\n. . . .\n. . . .\n. . . ." `shouldBe` Left "entry out of range: 5"
      readSudoku "2 2\n1 0 . .\n. . . .\n. . . .\n. . . ." `shouldBe` Left "entry out of range: 0"
      readSudoku "2 2\n1 1.0 . .\n. . . .\n. . . .\n. . . ." `shouldBe` Left "parse error: 1.0"
      readSudoku "2 2\na . . .\n. . . .\n. . . .\n. . . ." `shouldBe` Left "parse error: a"
      readSudoku "2 2\n1 -1 . .\n. . . .\n. . . .\n. . . ." `shouldBe` Left "entry out of range: -1"

    it "succeeds when input is valid" $ do
      (readSudoku "1 1\n." >>= Right . show) `shouldBe` Right "1 1\n.\n"
      (readSudoku "1 1\n.\n\n" >>= Right . show) `shouldBe` Right "1 1\n.\n"
      (readSudoku "1 1\n.\n \n" >>= Right . show) `shouldBe` Right "1 1\n.\n"
      (readSudoku "1 1\n1" >>= Right . show) `shouldBe` Right "1 1\n1\n"
      (readSudoku "2 2\n. . . .\n. . . .\n. . . .\n. . . ." >>= Right . show) `shouldBe` Right "2 2\n. . . .\n. . . .\n. . . .\n. . . .\n"
      (readSudoku "2 2\n1 2 . .\n. . . .\n. . . .\n. . . ." >>= Right . show) `shouldBe` Right "2 2\n1 2 . .\n. . . .\n. . . .\n. . . .\n"
      (readSudoku "2 2\n1 2 3 4\n3 4 1 2\n2 1 4 3\n4 3 2 1" >>= Right . show) `shouldBe` Right "2 2\n1 2 3 4\n3 4 1 2\n2 1 4 3\n4 3 2 1\n"
