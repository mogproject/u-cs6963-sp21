{-# LANGUAGE OverloadedStrings #-}

module Data.Board
  ( Pos,
    Workers,
    Players,
    Board (Board),
    Level,
    players,
    spaces,
    turn,
    readPlayers,
    readBoard,
    writePlayers,
    writeBoard,
    choosePlayers,
  )
where

import Control.Monad (mfilter)
import Data.Aeson (decode, encode)
import Data.Aeson.Types
import Data.String.Conversions (cs)
import Test.QuickCheck
import Data.List (nub)
import Data.List.Split (chunksOf)

-- dimension
type Dim = Int

type Level = Int

type Pos = (Dim, Dim)

type Workers = (Pos, Pos)

type Players = [Workers]

data Board = Board
  { players :: Players,
    spaces :: [[Level]],
    turn :: Int
  }
  deriving (Show, Eq)

instance FromJSON Board where
  parseJSON = withObject "board" $ \o -> Board <$> o .: "players" <*> o .: "spaces" <*> o .: "turn"

instance ToJSON Board where
  toJSON b = object ["players" .= players b, "spaces" .= spaces b, "turn" .= turn b]

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

isValidDim :: Dim -> Bool
isValidDim x = 1 <= x && x <= 5

isValidPos :: Pos -> Bool
isValidPos (x, y) = all isValidDim [x, y]

isUniquePos :: [Pos] -> Bool
isUniquePos ps = (== length ps) . length . nub $ ps

isValidLevel :: Level -> Bool
isValidLevel h = 0 <= h && h <= 4

isValidPlayers :: Bool -> Players -> Bool
isValidPlayers isStrict ws =
  let ps = concat [[w1, w2] | (w1, w2) <- ws]
   in ((if isStrict then (==) else (>=)) 2 . length) ws && all isValidPos ps && isUniquePos ps

isValidSpaces :: [[Level]] -> Bool
isValidSpaces sp = length sp == 5 && all (\r -> length r == 5 && all isValidLevel r) sp

isValidBoard :: Board -> Bool
isValidBoard b = isValidPlayers True (players b) && isValidSpaces (spaces b) && turn b >= 0

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

readPlayers :: String -> Maybe [Workers]
readPlayers = mfilter (isValidPlayers False) . decode . cs

readBoard :: String -> Maybe Board
readBoard = mfilter isValidBoard . decode . cs

writePlayers :: [Workers] -> String
writePlayers = cs . encode

writeBoard :: Board -> String
writeBoard = cs . encode

--------------------------------------------------------------------------------
-- Gen for property-based testing
--------------------------------------------------------------------------------

choosePlayers :: Int -> Gen Players
choosePlayers numPlayers = do
  positions <- choosePositions (numPlayers * 2)
  return $ createPlayers positions
  where
    choosePos :: Gen Pos
    choosePos = do
      x <- chooseInt (1, 5)
      y <- chooseInt (1, 5)
      return (x, y)
    choosePositions :: Int -> Gen [Pos]
    choosePositions k = do
      ls <- infiniteListOf choosePos
      return $ (take k . nub) ls
    createPlayers :: [Pos] -> Players
    createPlayers ps = [(w, u) | [w, u] <- chunksOf 2 ps]

instance Arbitrary Board where
  arbitrary = do
    p <- choosePlayers 2 -- possible that a worker is at level 4
    lv <- vectorOf 25 (chooseInt (0, 4)) -- not consistent with the turn
    t <- chooseInt (0, 100)
    return Board {players = p, spaces = chunksOf 5 lv, turn = t}
