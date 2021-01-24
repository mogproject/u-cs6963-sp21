import Algorithm.SudokuSolver (solveSudoku)
import Control.Monad ((>=>))
import Data.Sudoku (Sudoku, readSudoku)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case parseArgs args of
    Just seed -> interact $ either (++ "\n") show . (readSudoku >=> solve seed)
    Nothing -> putStr $ usage prog

-- | Parses command-line arguments.
parseArgs :: [String] -> Maybe Int
parseArgs args = case mapM readMaybe args of
  Just [seed] -> Just seed
  Just [] -> Just 0
  _ -> Nothing

-- | Command-line usage.
usage :: String -> String
usage p =
  unlines
    [ "Usage: " ++ p ++ " [<seed>]",
      "",
      "  seed : seed of the pseudo random number generator (default:0)"
    ]

-- | Solves the given Sudoku instance.
solve :: Int -> Sudoku -> Either String Sudoku
solve seed sd = case solveSudoku seed sd of
  x : _ -> Right x  -- returns the first solution
  _ -> Left "no solution"
