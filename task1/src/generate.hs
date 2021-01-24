import Algorithm.SudokuSolver (solveSudoku)
import Data.Sudoku (Sudoku, emptySudoku)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  putStr $ either id show $ maybe (Left (usage prog)) Right (parseArgs args) >>= generate

parseArgs :: [String] -> Maybe (Int, Int, Int)
parseArgs args = case mapM readMaybe args of
  Just [n, m, seed] | n > 0 && m > 0 -> Just (n, m, seed)
  Just [n, m] | n > 0 && m > 0 -> Just (n, m, 0)
  _ -> Nothing

usage :: String -> String
usage p =
  unlines
    [ "Usage: " ++ p ++ " <region_height> <region_width> [<seed>]",
      "",
      "  region_height: height of each region; positive integer",
      "  region_width : width of each region; positive integer",
      "  seed         : seed of the pseudo random number generator (default:0)"
    ]

generate :: (Int, Int, Int) -> Either String Sudoku
generate (n, m, seed) = case solveSudoku seed (emptySudoku n m) of
  x : _ -> Right x
  _ -> Left "failed to generate a sudoku instance"
