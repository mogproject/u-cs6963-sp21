import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case parseArgs args of
    Just seed -> print seed
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
