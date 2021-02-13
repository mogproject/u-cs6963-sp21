{-# LANGUAGE OverloadedStrings #-}

module Data.Board
  ( Pos,
    Workers,
    Players,
    Board (Board),
    players,
    spaces,
    turn,
    readPlayers,
    readBoard,
    writePlayers,
    writeBoard,
  )
where

import Control.Monad (foldM, mapM, mfilter)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Types
import Data.List
import Data.String.Conversions (cs)
import GHC.Generics

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
