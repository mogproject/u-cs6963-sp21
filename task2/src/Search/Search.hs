module Search.Search (findMove) where

import Data.Function (on)
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Map (Map, (!))
import qualified Data.Map
import Data.Ord (comparing)
import Game.Evaluation (Score, evaluate, evaluate', scoreWin)
import Game.GameState (GameMove, GameState (GameState), makeMove)
import qualified Game.GameState as GS

findMove :: Int -> Int -> GameState -> GameMove
-- Naive: choose the first legal move
findMove strategy seed GameState {GS.legalMoves = mv} | strategy == 1 && not (null mv) = head mv
findMove strategy seed g@GameState {GS.legalMoves = mv} | strategy == 2 && not (null mv) = searchMiniMax' g 2
findMove _ _ _ = undefined

-- Minimax
searchMiniMax' :: GameState -> Int -> GameMove
searchMiniMax' g depth = last . snd $ searchMiniMax g depth True []

searchMiniMax :: GameState -> Int -> Bool -> [GameMove] -> (Score, [GameMove])
-- reached depth limit
searchMiniMax g depth shouldMaximize sofar | depth == 0 = ((if shouldMaximize then 1 else -1) * evaluate g, sofar)
--
searchMiniMax g@GameState {GS.legalMoves = mv} depth shouldMaximize sofar =
  case evaluate' g of
    Just sc -> ((if shouldMaximize then 1 else -1) * sc, sofar) -- terminal node
    Nothing ->
      let nextMiniMaxScores = [searchMiniMax (makeMove g m) (depth -1) (not shouldMaximize) (m : sofar) | m <- mv] :: [(Score, [GameMove])]
       in (if shouldMaximize then maximumBy else minimumBy) (comparing fst) nextMiniMaxScores

-- Alpha-beta search
-- searchAlphaBeta :: GameState -> Int -> GameMove
-- searchAlphaBeta st depth = last $ searchAlphaBeta' st depth (- scoreWin -1) (scoreWin + 1) True []

-- searchAlphaBeta' :: GameState -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove])
-- searchAlphaBeta' st depth alpha beta shouldMaximize sofar | depth == 0 = sofar
-- searchAlphaBeta' g@GameState {GS.legalMoves = mv} depth alpha beta shouldMaximize sofar =
--   let nextStates = [(- (evaluate st), (st, m)) | m <- mv, let st = makeMove g m]
--       nextStatesSorted = sortBy ((if shouldMaximize then flip else id) compare `on` fst) nextStates
--    in []

-- -- -- find the best move in the candidates
-- searchAlphaBeta'' :: GameState -> Int -> Score -> Score -> Bool -> [(Score, GameMove, GameState)] -> (Score, GameMove) -> (Score, GameMove)
-- searchAlphaBeta'' st depth alpha beta shouldMaximize ((sc, mv, ns) :: candidates) best@(bestScore, bestMove) =
--   if (bestScore < sc && shouldMaximize) || (bestScore > sc && not shouldMaximize)
--     then
--       let nextBest = (sc, mv)
--           nextAlpha = if shouldMaximize then max alpha sc else alpha
--           nextBeta = if shouldMaximize then beta else min beta sc
--        in if nextBeta <= nextAlpha then nextBest else searchAlphaBeta'' st depth nextAlpha nextBeta shouldMaximize candidates nextBest
--     else searchAlphaBeta'' st depth alpha beta shouldMaximize candidates best
