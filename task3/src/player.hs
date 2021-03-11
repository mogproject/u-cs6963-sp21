import Data.Aeson (encode)
import qualified Data.Board as B
import qualified Data.BoardWithoutCard as B'
import Data.String.Conversions (cs)
import Game.GameState (fromBoard, fromBoardWithoutCard, makeMove, toBoard, toBoardWithoutCard)
import Search.Initial (findStartingPlayer1, findStartingPlayer2)
import Search.Search (findMove, findMoveWithTimeout)
import System.Environment (getArgs, getProgName)
import System.IO (BufferMode (LineBuffering), hSetBuffering, isEOF, stdout)
import System.Random (StdGen, getStdGen, mkStdGen)
import Text.Read (readMaybe)

-- | Entry point of the program.
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case parseArgs args (Just defaultParams) of
    Nothing -> do
      putStr $ usage prog
    Just p -> do
      hSetBuffering stdout LineBuffering -- make sure to flush each line
      mainLoop p 0

-- | Main loop.
mainLoop :: Params -> Int -> IO ()
mainLoop p@Params {seed = sd, timeout = to, depth = d} lineNo = do
  line <- getLine
  stdGen <- maybe getStdGen (return . mkStdGen) sd
  out <- processLine stdGen to d lineNo line
  putStrLn out
  done <- isEOF
  if done
    then return ()
    else mainLoop p (lineNo + 1)

-- | Parses command-line arguments.
data Params = Params {help :: Bool, seed :: Maybe Int, timeout :: Maybe Int, depth :: Maybe Int}

defaultParams :: Params
defaultParams = Params {help = False, seed = Nothing, timeout = Just 30, depth = Nothing}

parseArgs :: [String] -> Maybe Params -> Maybe Params
parseArgs [] p = p
parseArgs ("--help" : _) _ = Nothing
parseArgs ("--seed" : s : ss) (Just Params {help = False, timeout = t, depth = d}) = case readMaybe s of
  Just x -> parseArgs ss (Just Params {help = False, seed = Just x, timeout = t, depth = d})
  Nothing -> Nothing
parseArgs ("--timeout" : s : ss) (Just Params {help = False, seed = sd, depth = d}) = case readMaybe s of
  Just x -> parseArgs ss (Just Params {help = False, seed = sd, timeout = Just (max 0 x), depth = d})
  Nothing -> Nothing
parseArgs ("--depth" : s : ss) (Just Params {help = False, seed = sd, timeout = t}) = case readMaybe s of
  Just x -> parseArgs ss (Just Params {help = False, seed = sd, timeout = t, depth = Just (max 1 x)})
  Nothing -> Nothing
parseArgs _ _ = Nothing

-- | Command-line usage.
usage :: String -> String
usage p =
  unlines
    [ "Usage: " ++ p ++ " [--seed <seed>] [--timeout <timeout>] [--depth <depth>]",
      "",
      "  seed    : seed of the pseudo random number generator (default:None)",
      "  timeout : timeout for each move in seconds (default:10)",
      "  depth   : maximum search depth (default:None)"
    ]

-- | Adds some buffer.
adjustTimeout :: Int -> Int
adjustTimeout x = max (x * 700000) ((x - 2) * 1000000)

processLine :: StdGen -> Maybe Int -> Maybe Int -> Int -> String -> IO String
processLine gen to dep lineNo line = case lineNo of
  0 -> return $ case B.readPlayers line of
    Just (B.Player {B.card = c1, B.tokens = Nothing}, p2@B.Player {B.card = c2, B.tokens = Nothing}) ->
      -- Player 1 with card
      cs . encode $ (p2, B.Player {B.card = c1, B.tokens = Just $ findStartingPlayer1 1 gen (Just (c1, c2))})
    Just (B.Player {B.card = c1, B.tokens = Nothing}, p2@B.Player {B.card = c2, B.tokens = Just ws}) ->
      -- Player 2 with card
      cs . encode $ (p2, B.Player {B.card = c1, B.tokens = Just $ findStartingPlayer2 1 gen ws (Just (c1, c2))})
    _ -> case B'.readPlayers line of
      -- Player 1 without card
      Just [] -> cs . encode $ [findStartingPlayer1 1 gen Nothing]
      -- Player 2 without card
      Just [p] -> cs . encode $ [p, findStartingPlayer2 1 gen p Nothing]
      _ -> "unexpected input"
  _ -> case B.readBoard line of
    Just b ->
      -- Board with card
      let st = fromBoard b
       in case to of
            Nothing -> do
              return $ cs . encode . toBoard . makeMove st $ findMove 4 gen dep st
            Just t -> do
              mv <- findMoveWithTimeout (adjustTimeout t) dep st
              return $ cs . encode . toBoard . makeMove st $ mv
    _ -> case B'.readBoard line of
      Just b ->
        -- Board without card
        let st = fromBoardWithoutCard b
         in case to of
              Nothing -> do
                return $ cs . encode . toBoardWithoutCard . makeMove st $ findMove 4 gen dep st
              Just t -> do
                mv <- findMoveWithTimeout (adjustTimeout t) dep st
                return $ cs . encode . toBoardWithoutCard . makeMove st $ mv
      _ -> return "unexpected input"
