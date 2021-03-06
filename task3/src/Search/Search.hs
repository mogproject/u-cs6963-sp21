module Search.Search (findMove, searchAlphaBeta, searchAlphaBetaNaive, findMoveAlphaBeta, findMoveWithTimeout) where

import Control.Concurrent (forkIO, killThread, threadDelay)
-- import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.MVar (newMVar, swapMVar, takeMVar)
import qualified Control.Exception
import Data.List (maximumBy, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
-- import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Game.Evaluation (Score, evaluate, evaluate', scoreWin)
import Game.GameMove
import Game.GameState (GameState (GameState), getLegalMoves', makeMove)
import qualified Game.GameState as GS
import System.Random (StdGen)

findMove :: Int -> StdGen -> Maybe Int -> GameState -> GameMove
-- Only one choise
findMove _ _ _ GameState {GS.legalMoves = [m]} = m
-- Naive: choose the first legal move
findMove 1 _ _ GameState {GS.legalMoves = m : _} = m
-- Minimax
findMove 2 _ (Just depth) g@GameState {GS.legalMoves = _ : _} = searchMiniMax g depth
-- Alpha-beta (generic): Do not use. Why is this so slow?
findMove 3 _ (Just depth) g = getNextMove g $ findMoveAlphaBeta g depth
-- Alpha-beta
findMove 4 _ (Just depth) g = snd $ searchAlphaBetaNaive g depth
-- (unexpected strategy or no valid moves)
findMove _ _ _ _ = undefined

findMoveWithTimeout :: Int -> Maybe Int -> GameState -> IO GameMove
findMoveWithTimeout timeoutMicroSeconds depthLimit g = do
  -- Create a synchronized mutable variable.
  mvar <- newMVar $ head (getLegalMoves' False g)

  -- Define a function.
  let compute depth = do
        (sc, x) <- Control.Exception.evaluate $ searchAlphaBetaNaive g depth
        -- print $ "depth=" ++ show depth ++ ", score=" ++ show sc ++ ", move=" ++ show x
        _ <- swapMVar mvar $! x
        if sc == scoreWin || sc == (- scoreWin) || maybe False (depth >=) depthLimit
          then do
            -- finish search
            return ()
          else compute $ depth + 1

  -- Start a new thread.
  tid <- forkIO (compute 1)

  -- Wait and kill the thread.
  threadDelay timeoutMicroSeconds
  killThread tid

  -- Get the final result.
  takeMVar mvar

--------------------------------------------------------------------------------
-- Wrappers
--------------------------------------------------------------------------------

type SearchNode = (GameState, Maybe GameMove)

createRoot :: GameState -> SearchNode
createRoot s = (s, Nothing)

createBranches :: SearchNode -> [SearchNode]
createBranches (s@GameState {GS.legalMoves = mv}, _) =
  case evaluate' s of
    (Just _, _) -> [] -- has a conclusion
    (_, _) -> [(GS.makeMove s m, Just m) | m <- mv]

scoreNode :: SearchNode -> Score
scoreNode = evaluate . fst

getNextMove :: GameState -> (Score, [SearchNode]) -> GameMove
getNextMove s (_, xs) = if null xs then head (getLegalMoves' False s) else fromMaybe undefined $ (last . map snd) xs

findMoveAlphaBeta :: GameState -> Int -> (Score, [SearchNode])
findMoveAlphaBeta g@GameState {GS.turn = t, GS.legalMoves = _ : _} depth = searchAlphaBeta createBranches scoreNode (- scoreWin) scoreWin (createRoot g) (even t) depth
findMoveAlphaBeta GameState {GS.turn = t, GS.legalMoves = []} _ = (if even t then - scoreWin else scoreWin, [])

--------------------------------------------------------------------------------
-- Minimax Search
--------------------------------------------------------------------------------

searchMiniMax :: GameState -> Int -> GameMove
searchMiniMax g depth = last . snd $ searchMiniMax' g depth True []

searchMiniMax' :: GameState -> Int -> Bool -> [GameMove] -> (Score, [GameMove])
-- reached depth limit
searchMiniMax' g depth shouldMaximize sofar | depth == 0 = ((if shouldMaximize then 1 else -1) * evaluate g, sofar)
--
searchMiniMax' g@GameState {GS.legalMoves = mv} depth shouldMaximize sofar =
  case evaluate' g of
    (Just sc, Nothing) -> ((if shouldMaximize then 1 else -1) * sc, sofar) -- terminal node
    (Just sc, Just m) -> ((if shouldMaximize then 1 else -1) * sc, m : sofar) -- terminal node
    (Nothing, _) ->
      let nextMiniMaxScores = [searchMiniMax' (makeMove g m) (depth -1) (not shouldMaximize) (m : sofar) | m <- mv] :: [(Score, [GameMove])]
       in (if shouldMaximize then maximumBy else minimumBy) (comparing fst) nextMiniMaxScores

--------------------------------------------------------------------------------
-- Alpha-beta Search
--------------------------------------------------------------------------------

searchAlphaBetaNaive :: GameState -> Int -> (Score, GameMove)
searchAlphaBetaNaive g@GameState {GS.turn = t} depth =
  let (score, result) = searchAlphaBetaNaive' g depth (- scoreWin) scoreWin (even t) []
   in (score, if null result then head (getLegalMoves' False g) else last result)

searchAlphaBetaNaive' :: GameState -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove])
-- reached depth limit
searchAlphaBetaNaive' g depth _ _ _ sofar
  | depth <= 0 = (evaluate g, sofar)
--
searchAlphaBetaNaive' g@GameState {GS.legalMoves = mv} depth alpha beta shouldMaximize sofar =
  case evaluate' g of
    (Just sc, Nothing) -> (sc, sofar) -- terminal node
    (Just sc, Just m) -> (sc, m : sofar) -- terminal node
    (Nothing, _) ->
      let nextStates = fmap (makeMove g) mv --(take (depth * 20) mv) -- branch cut
          bestScore = if shouldMaximize then - scoreWin - 1 else scoreWin + 1
       in searchAlphaBetaNaive'' (zip mv nextStates) depth alpha beta shouldMaximize sofar (bestScore, [])

searchAlphaBetaNaive'' :: [(GameMove, GameState)] -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove]) -> (Score, [GameMove])
searchAlphaBetaNaive'' [] _ _ _ _ _ best = best
searchAlphaBetaNaive'' ((mv, st) : gs) depth alpha beta shouldMaximize sofar (bestScore, bestMove) =
  let (value, moves) = searchAlphaBetaNaive' st (depth -1) alpha beta (not shouldMaximize) (mv : sofar)
   in if (shouldMaximize && value > bestScore) || (not shouldMaximize && value < bestScore)
        then do
          let best' = (value, moves)
          let alpha' = if shouldMaximize then max alpha value else alpha
          let beta' = if shouldMaximize then beta else min beta value
          if beta' <= alpha'
            then best' -- cutoff
            else searchAlphaBetaNaive'' gs depth alpha' beta' shouldMaximize sofar best'
        else searchAlphaBetaNaive'' gs depth alpha beta shouldMaximize sofar (bestScore, bestMove)

--------------------------------------------------------------------------------
-- Alpha-beta Search (Generic)
--------------------------------------------------------------------------------

searchAlphaBeta :: Ord score => (node -> [node]) -> (node -> score) -> score -> score -> node -> Bool -> Int -> (score, [node])
searchAlphaBeta branch score minValue maxValue root maximize depth = searchAlphaBeta' root branch score depth minValue maxValue maximize []

-- move vertically to the next level
searchAlphaBeta' :: Ord score => node -> (node -> [node]) -> (node -> score) -> Int -> score -> score -> Bool -> [node] -> (score, [node])
searchAlphaBeta' root _ score depth _ _ _ sofar | depth <= 0 = (score root, sofar) -- depth limit
searchAlphaBeta' root branch score depth alpha beta maximize sofar =
  case branch root of
    [] -> (score root, sofar) -- leaf node
    xs ->
      let f = (\x -> searchAlphaBeta' x branch score (depth - 1) alpha beta (not maximize) (x : sofar))
       in searchAlphaBeta'' f xs alpha beta maximize sofar Nothing

-- move horizontally to the next candidate
searchAlphaBeta'' :: Ord score => (node -> (score, [node])) -> [node] -> score -> score -> Bool -> [node] -> Maybe (score, [node]) -> (score, [node])
searchAlphaBeta'' f (x : xs) alpha beta maximize sofar best =
  let best'@(z, _) = f x
   in if maybe True (\(y, _) -> (if maximize then (>) else (<)) z y) best -- best value updated
        then
          let (alpha', beta') = if maximize then (max alpha z, beta) else (alpha, min beta z)
           in if beta' <= alpha' then best' else searchAlphaBeta'' f xs alpha' beta' maximize sofar (Just best')
        else searchAlphaBeta'' f xs alpha beta maximize sofar best
searchAlphaBeta'' _ [] _ _ _ _ (Just best) = best -- examined all candidates
searchAlphaBeta'' _ [] _ _ _ _ _ = undefined -- never happens
