import Data.Aeson (encode)
import qualified Data.Board as B
import qualified Data.BoardWithoutCard as B'
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Game.GameState (fromBoard, fromBoardWithoutCard, makeMove, toBoard, toBoardWithoutCard)
import Search.Initial (findStartingPlayer1, findStartingPlayer2)
import Search.Search (findMove)
import System.Environment (getArgs, getProgName)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
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
  0 -> case B.readPlayers line of
    Just (B.Player {B.card = c1, B.tokens = Nothing}, p2@B.Player {B.card = c2, B.tokens = Nothing}) ->
      -- Player 1 with card
      cs . encode $ (p2, B.Player {B.card = c1, B.tokens = Just $ findStartingPlayer1 1 s (Just (c1, c2))})
    Just (B.Player {B.card = c1, B.tokens = Nothing}, p2@B.Player {B.card = c2, B.tokens = Just ws}) ->
      -- Player 2 with card
      cs . encode $ (p2, B.Player {B.card = c1, B.tokens = Just $ findStartingPlayer2 1 s ws (Just (c1, c2))})
    _ -> case B'.readPlayers line of
      -- Player 1 without card
      Just [] -> cs . encode $ [findStartingPlayer1 1 s Nothing]
      -- Player 2 without card
      Just [p] -> cs . encode $ [p, findStartingPlayer2 1 s p Nothing]
      _ -> "unexpected input"
  _ -> case B.readBoard line of
    Just b ->
      -- TODO: Refactor logic.
      -- Board with card
      let st = fromBoard b
       in cs . encode . toBoard . makeMove st $ findMove 3 s st
    _ -> case B'.readBoard line of
      Just b ->
        -- Board without card
        let st = fromBoardWithoutCard b
         in cs . encode . toBoardWithoutCard . makeMove st $ findMove 3 s st
      _ -> "unexpected input"
