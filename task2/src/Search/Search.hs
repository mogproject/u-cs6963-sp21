module Search.Search (findMove) where

import Game.GameState (GameMove, GameState (GameState))
import qualified Game.GameState as GS

findMove :: Int -> Int -> GameState -> GameMove

-- Naive: choose the first legal move
findMove strategy seed GameState {GS.players = _, GS.levels = _, GS.turn = _, GS.legalMoves = mv} | strategy == 1 = head mv


findMove _ _ _ = undefined
