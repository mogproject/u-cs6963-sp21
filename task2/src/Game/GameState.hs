module Game.GameState
where

import Data.Board (Board)


type GameMove = Int

-- Internal representation of game states.
data GameState = GameState 

fromBoard :: Board -> GameState
fromBoard _ = GameState 
