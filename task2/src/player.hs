import Data.Aeson (encode)
import Data.Board (readBoard, readPlayers)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Game.GameState (fromBoard, makeMove, toBoard)
import Search.Initial (findStartingPlayer1, findStartingPlayer2)
import Search.Search (findMove)
import System.Environment (getArgs, getProgName)
import System.IO
import Text.Read (readMaybe)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let Params {help = h, seed = s} = parseArgs args Params {help = False, seed = Nothing}
  if h
    then do
      putStr $ usage prog
    else do
      hSetBuffering stdout LineBuffering -- make sure to flush each line
      interact $ unlines . zipWith (processLine (fromMaybe 0 s)) [0 ..] . lines

-- | Parses command-line arguments.
data Params = Params {help :: Bool, seed :: Maybe Int}

parseArgs :: [String] -> Params -> Params
parseArgs [] p = p
parseArgs ("--help" : _) _ = Params {help = True, seed = Nothing}
parseArgs (s : ss) Params {help = False, seed = Nothing} = case readMaybe s of
  Just x -> parseArgs ss Params {help = False, seed = x}
  Nothing -> Params {help = True, seed = Nothing}
parseArgs _ _ = Params {help = True, seed = Nothing}

-- | Command-line usage.
usage :: String -> String
usage p =
  unlines
    [ "Usage: " ++ p ++ " [<seed>]",
      "",
      "  seed : seed of the pseudo random number generator (default:0)"
    ]

processLine :: Int -> Int -> String -> String
processLine s lineNo line = case lineNo of
  0 -> case readPlayers line of
    Just [] -> cs . encode $ [findStartingPlayer1 1 s]
    Just [p] -> cs . encode $ [p, findStartingPlayer2 1 s p]
    _ -> "unexpected input"
  _ -> case readBoard line of
    Just b ->
      let st = fromBoard b
       in cs . encode . toBoard . makeMove st $ findMove 2 s st
    _ -> "unexpected input"
