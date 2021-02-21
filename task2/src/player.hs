import Data.Aeson (decode, encode)
import Data.String.Conversions (cs)
import Data.Board (readBoard, readPlayers)
import Data.List
import System.Environment (getArgs, getProgName)
import System.IO
import System.Random (StdGen, mkStdGen, randomR)
import Text.Read (readMaybe)
import Search.Initial (findStartingPlayer1, findStartingPlayer2)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  hSetBuffering stdout LineBuffering -- make sure to flush each line
  interact $ unlines . zipWith (processLine 123) [0 ..] . lines

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

processLine :: Int -> Int -> String -> String
processLine seed lineNo line = case lineNo of
  0 -> case readPlayers line of
    Just [] -> (cs . encode) [findStartingPlayer1 1 seed]
    Just [p] -> (cs . encode) [p, findStartingPlayer2 1 seed p]
    _ -> "unexpected input"
  _ -> case readBoard line of
    Just b -> "ok"
    _ -> "unexpected input"
