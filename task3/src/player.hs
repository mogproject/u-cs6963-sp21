import Data.Aeson (encode)
import qualified Data.Board as B
import qualified Data.BoardWithoutCard as B'
import Data.String.Conversions (cs)
import Game.GameState (fromBoard, fromBoardWithoutCard, makeMove, toBoard, toBoardWithoutCard)
import Search.Initial (findStartingPlayer1, findStartingPlayer2)
import Search.Search (findMove, findMoveWithTimeout)
import System.Environment (getArgs, getProgName)
import System.IO (BufferMode (LineBuffering), hSetBuffering, isEOF, stdout)
import Text.Read (readMaybe)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let p@Params {help = h} = parseArgs args Params {help = False, seed = 0, timeout = 5}
  if h
    then do
      putStr $ usage prog
    else do
      hSetBuffering stdout LineBuffering -- make sure to flush each line
      mainLoop p 0

-- | Main loop.
mainLoop :: Params -> Int -> IO ()
mainLoop p@Params {seed = sd, timeout = to} lineNo = do
  line <- getLine
  out <- processLine sd to lineNo line
  putStrLn out
  done <- isEOF
  if done
    then return ()
    else mainLoop p (lineNo + 1)

-- | Parses command-line arguments.
data Params = Params {help :: Bool, seed :: Int, timeout :: Int}

parseArgs :: [String] -> Params -> Params
parseArgs [] p = p
parseArgs ("--help" : _) _ = Params {help = True, seed = 0, timeout = 0}
parseArgs ("--seed" : s : ss) Params {help = False, timeout = t} = case readMaybe s of
  Just x -> parseArgs ss Params {help = False, seed = x, timeout = t}
  Nothing -> Params {help = True, seed = 0, timeout = 0}
parseArgs ("--timeout" : s : ss) Params {help = False, seed = sd} = case readMaybe s of
  Just x -> parseArgs ss Params {help = False, seed = sd, timeout = max 0 x}
  Nothing -> Params {help = True, seed = 0, timeout = 0}
parseArgs _ _ = Params {help = True, seed = 0, timeout = 0}

-- | Command-line usage.
usage :: String -> String
usage p =
  unlines
    [ "Usage: " ++ p ++ " [<seed>] [--timeout <timeout>]",
      "",
      "  seed : seed of the pseudo random number generator (default:0)",
      "  timeout : timeout for each move in seconds (default:None)"
    ]

adjustTimeout :: Int -> Int
adjustTimeout x = max (x * 1000000 `div` 2) ((x - 2) * 1000000)

processLine :: Int -> Int -> Int -> String -> IO String
processLine s to lineNo line = case lineNo of
  0 -> return $ case B.readPlayers line of
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
       in if to == 0
            then return $ cs . encode . toBoard . makeMove st $ findMove 4 s st
            else do
              mv <- findMoveWithTimeout (adjustTimeout to) st
              return $ cs . encode . toBoard . makeMove st $ mv
    _ -> case B'.readBoard line of
      Just b ->
        -- Board without card
        let st = fromBoardWithoutCard b
         in if to == 0
              then return $ cs . encode . toBoardWithoutCard . makeMove st $ findMove 4 s st
              else do
                mv <- findMoveWithTimeout (adjustTimeout to) st
                return $ cs . encode . toBoardWithoutCard . makeMove st $ mv
      _ -> return "unexpected input"
