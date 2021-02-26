{-# LANGUAGE OverloadedStrings #-}

module Data.Card
  ( Card (Apollo, Artemis, Atlas, Demeter, Hephastus, Minotaur, Pan, Prometheus),
  )
where

import Data.Aeson.Types
import Data.List (find)
import Data.String.Conversions (cs)

data Card
  = Apollo
  | Artemis
  | Atlas
  | Demeter
  | Hephastus
  | Minotaur
  | Pan
  | Prometheus
  deriving (Enum, Show, Eq)

instance FromJSON Card where
  parseJSON =
    withText
      "card"
      ( \s -> case find (\x -> show x == cs s) [Apollo ..] of
          Just x -> return x
          Nothing -> fail $ "Unknown card name: " ++ cs s
      )

instance ToJSON Card where
  toJSON c = String $ cs (show c)
