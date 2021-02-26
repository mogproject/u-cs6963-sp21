{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Data.Board
  ( Pos,
    Workers,
    Player (Player),
    Players,
    Board (Board),
    Level,
    players,
    card,
    tokens,
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
import Data.Card (Card (Apollo, Minotaur))
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.String.Conversions (cs)
import GHC.Generics
import Test.QuickCheck (Arbitrary, Gen, arbitrary, chooseInt, shuffle, vectorOf)

-- dimension
type Dim = Int

type Level = Int

type Pos = (Dim, Dim)

type Workers = (Pos, Pos)

data Player = Player
  { card :: Card,
    tokens :: Maybe Workers
  }
  deriving (Show, Eq, Generic)

type Players = (Player, Player)

data Board = Board
  { players :: Players,
    spaces :: [[Level]],
    turn :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Player where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance ToJSON Player where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- instance FromJSON Player where
-- parseJSON = withObject "player" $ \o -> Player <$> o .: "card" <*> o .:? "tokens"

-- instance ToJSON Player where
-- toJSON p = object $ ["card" .= card p] ++ ["tokens" .= x | x <- maybeToList (tokens p)]

instance FromJSON Board where
  parseJSON = genericParseJSON defaultOptions
  -- parseJSON = withObject "board" $ \o -> Board <$> o .: "players" <*> o .: "spaces" <*> o .: "turn"

instance ToJSON Board where
  toJSON = genericToJSON defaultOptions
  -- object ["players" .= players b, "spaces" .= spaces b, "turn" .= turn b]
  -- toJSON b = object ["players" .= players b, "spaces" .= spaces b, "turn" .= turn b]

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
isValidPlayers isPreGame (p1, p2) =
  let isValidCard = all (\c -> card c `elem` [Apollo ..]) [p1, p2]
      isTokenDefined = all (isJust . tokens) [p1, p2]
      ps = concatMap (\(w, u) -> [w, u]) $ mapMaybe tokens [p1, p2]
   in isValidCard && (isPreGame || isTokenDefined) && all isValidPos ps && isUniquePos ps

isValidSpaces :: [[Level]] -> Bool
isValidSpaces sp = length sp == 5 && all (\r -> length r == 5 && all isValidLevel r) sp

isValidBoard :: Board -> Bool
isValidBoard b = isValidPlayers False (players b) && isValidSpaces (spaces b) && turn b >= 0

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

readPlayers :: String -> Maybe Players
readPlayers = mfilter (isValidPlayers True) . decode . cs

writePlayers :: Players -> String
writePlayers = cs . encode

readBoard :: String -> Maybe Board
readBoard = mfilter isValidBoard . decode . cs

writeBoard :: Board -> String
writeBoard = cs . encode

--------------------------------------------------------------------------------
-- Gen for property-based testing
--------------------------------------------------------------------------------

-- Chooses distinct n elements from a finite list.
chooseDistinct :: [a] -> Int -> Gen [a]
chooseDistinct xs n = do
  ys <- shuffle xs
  return $ take n ys

choosePlayers :: Int -> Gen Players
choosePlayers n = do
  cards <- chooseDistinct [Apollo ..] 2
  ps <- chooseDistinct [0 .. 24] (n * 2)
  return
    ( Player {card = cards !! 0, tokens = if n == 2 then Just (toPos (ps !! 2), toPos (ps !! 3)) else Nothing},
      Player {card = cards !! 1, tokens = if n >= 1 then Just (toPos (ps !! 0), toPos (ps !! 1)) else Nothing}
    )
  where
    toPos i = (i `div` 5 + 1, i `mod` 5 + 1)

chooseWorkerLevels :: Level -> Gen [Level]
chooseWorkerLevels maxLevel = vectorOf 2 $ chooseInt (0, maxLevel)

instance Arbitrary Board where
  arbitrary =
    let fromPos (r, c) = (r - 1) * 5 + (c - 1)
     in do
          p@(p1, p2) <- choosePlayers 2
          let c1 = card p1
          let (w1, w2) = fromMaybe undefined (tokens p1)
          let c2 = card p2
          let (w3, w4) = fromMaybe undefined (tokens p2)
          lv1 <- chooseWorkerLevels (if c2 == Minotaur then 3 else 2)
          lv2 <- chooseWorkerLevels (if c1 == Minotaur then 3 else 2)
          lvs <- vectorOf 25 (chooseInt (0, 4))
          let lm1 = Data.Map.fromList $ zip [0 ..] lvs

          -- update workers' levels
          let lm2 = Data.Map.insert (fromPos w1) (lv1 !! 0) lm1
          let lm3 = Data.Map.insert (fromPos w2) (lv1 !! 1) lm2
          let lm4 = Data.Map.insert (fromPos w3) (lv2 !! 0) lm3
          let lm5 = Data.Map.insert (fromPos w4) (lv2 !! 1) lm4

          let lv = (map snd . sort) $ Data.Map.toList lm5

          -- it's possible that there are no legal moves
          return Board {players = p, spaces = chunksOf 5 lv, turn = sum lv}
