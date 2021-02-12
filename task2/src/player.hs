import Data.Aeson (decode)
-- import Data.String.Conversions (cs)

import Data.Board (readBoard, readPlayers)
import Data.List
import System.Environment (getArgs, getProgName)
import System.IO
import Text.Read (readMaybe)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  hSetBuffering stdout LineBuffering -- make sure to flush each line
  interact $ unlines . map processLine . zip [0 ..] . lines

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

processLine :: (Int, String) -> String
processLine (lineNo, line) = case lineNo of
  0 -> case readPlayers line of
    Just [] -> "I'm Player 1"
    Just [p] -> "I'm Player 2"
    _ -> "unexpected input"
  _ -> (show . readBoard) line
