module Search.Search (findMove, searchAlphaBetaGeneric, searchAlphaBeta, fetchNextMove, findMoveAlphaBeta, findMoveWithTimeout) where

import Control.Concurrent (forkIO, killThread, threadDelay)
-- import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, swapMVar, takeMVar)
import qualified Control.Exception
-- import Data.Maybe (fromMaybe)

-- import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)

-- import Control.Monad (forM_)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Game.Evaluation (Score, evaluate, evaluate', scoreWin)
import Game.GameMove
import Game.GameState (GameState (GameState, turn), getLegalMoves', makeMove)
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
-- findMove 3 _ (Just depth) g = getNextMove g $ findMoveAlphaBeta g depth
-- -- Alpha-beta
findMove 4 _ (Just depth) g = fetchNextMove g $ searchAlphaBeta g depth
-- (unexpected strategy or no valid moves)
findMove _ _ _ _ = undefined

findMoveWithTimeout :: Int -> Maybe Int -> GameState -> IO GameMove
findMoveWithTimeout timeoutMicroSeconds depthLimit g = do
  -- Create a synchronized mutable variable.
  mvar <- newMVar $ head (getLegalMoves' False g)
  done <- newEmptyMVar

  -- Define a function.
  let compute depth = do
        result@(sc, _) <- Control.Exception.evaluate $ searchAlphaBeta g depth

        -- DEBUG
        -- putStrLn $ "[DEBUG] AlphaBeta: depth=" ++ show depth ++ ", score=" ++ show sc ++ ", moves=["
        -- forM_ (reverse xs) (\x -> putStr $ "  " ++ showMove x ++ ",\n")
        -- putStrLn "]"

        x <- Control.Exception.evaluate $ fetchNextMove g result
        if sc == scoreWin * (if even (turn g) then (-1) else 1)
          then do
            putMVar done True
            return () -- do not update the move if it finds the loss because it tends to a worse move
          else do
            _ <- swapMVar mvar $! x

            if sc == scoreWin || sc == (- scoreWin) || maybe False (depth >=) depthLimit
              then do
                -- finish search
                putMVar done True
                return ()
              else compute $ depth + 1

  let timer = do
        threadDelay timeoutMicroSeconds
        putMVar done True
        return ()

  -- Start new threads.
  tid <- forkIO (compute 1)
  _ <- forkIO timer

  -- Wait and kill the thread.
  _ <- takeMVar done
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

-- getNextMove :: GameState -> (Score, [SearchNode]) -> GameMove
-- getNextMove s (_, xs) = if null xs then head (getLegalMoves' False s) else fromMaybe undefined $ (last . map snd) xs

findMoveAlphaBeta :: GameState -> Int -> (Score, [SearchNode])
findMoveAlphaBeta g@GameState {GS.turn = t, GS.legalMoves = _ : _} depth = searchAlphaBetaGeneric createBranches scoreNode (- scoreWin) scoreWin (createRoot g) (even t) depth
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
alphaBetaMaxWidth :: Int
alphaBetaMaxWidth = 150

fetchNextMove :: GameState -> (Score, [GameMove]) -> GameMove
fetchNextMove g (_, []) = head (getLegalMoves' False g)
fetchNextMove _ (_, result) = last result

searchAlphaBeta :: GameState -> Int -> (Score, [GameMove])
searchAlphaBeta g@GameState {GS.turn = t} depth = searchAlphaBeta' g depth (- scoreWin) scoreWin (even t) []

searchAlphaBeta' :: GameState -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove])
-- reached depth limit
searchAlphaBeta' g depth _ _ _ sofar
  | depth <= 0 = (evaluate g, sofar)
--
searchAlphaBeta' g@GameState {GS.legalMoves = mv} depth alpha beta shouldMaximize sofar =
  case evaluate' g of
    (Just sc, Nothing) -> (sc, sofar) -- terminal node
    (Just sc, Just m) -> (sc, m : sofar) -- terminal node
    (Nothing, _) ->
      let nextStates = fmap (makeMove g) (take alphaBetaMaxWidth mv) -- branch cut
          bestScore = if shouldMaximize then - scoreWin - 1 else scoreWin + 1
       in searchAlphaBeta'' (zip mv nextStates) depth alpha beta shouldMaximize sofar (bestScore, [])

searchAlphaBeta'' :: [(GameMove, GameState)] -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove]) -> (Score, [GameMove])
searchAlphaBeta'' [] _ _ _ _ _ best = best
searchAlphaBeta'' ((mv, st) : gs) depth alpha beta shouldMaximize sofar (bestScore, bestMove) =
  let (value, moves) = searchAlphaBeta' st (depth -1) alpha beta (not shouldMaximize) (mv : sofar)
   in if (shouldMaximize && value > bestScore) || (not shouldMaximize && value < bestScore)
        then do
          let best' = (value, moves)
          let alpha' = if shouldMaximize then max alpha value else alpha
          let beta' = if shouldMaximize then beta else min beta value
          if beta' <= alpha'
            then best' -- cutoff
            else searchAlphaBeta'' gs depth alpha' beta' shouldMaximize sofar best'
        else searchAlphaBeta'' gs depth alpha beta shouldMaximize sofar (bestScore, bestMove)

--------------------------------------------------------------------------------
-- Alpha-beta Search (Generic)
--------------------------------------------------------------------------------

searchAlphaBetaGeneric :: Ord score => (node -> [node]) -> (node -> score) -> score -> score -> node -> Bool -> Int -> (score, [node])
searchAlphaBetaGeneric branch score minValue maxValue root maximize depth = searchAlphaBetaGeneric' root branch score depth minValue maxValue maximize []

-- move vertically to the next level
searchAlphaBetaGeneric' :: Ord score => node -> (node -> [node]) -> (node -> score) -> Int -> score -> score -> Bool -> [node] -> (score, [node])
searchAlphaBetaGeneric' root _ score depth _ _ _ sofar | depth <= 0 = (score root, sofar) -- depth limit
searchAlphaBetaGeneric' root branch score depth alpha beta maximize sofar =
  case branch root of
    [] -> (score root, sofar) -- leaf node
    xs ->
      let f = (\x -> searchAlphaBetaGeneric' x branch score (depth - 1) alpha beta (not maximize) (x : sofar))
       in searchAlphaBetaGeneric'' f xs alpha beta maximize sofar Nothing

-- move horizontally to the next candidate
searchAlphaBetaGeneric'' :: Ord score => (node -> (score, [node])) -> [node] -> score -> score -> Bool -> [node] -> Maybe (score, [node]) -> (score, [node])
searchAlphaBetaGeneric'' f (x : xs) alpha beta maximize sofar best =
  let best'@(z, _) = f x
   in if maybe True (\(y, _) -> (if maximize then (>) else (<)) z y) best -- best value updated
        then
          let (alpha', beta') = if maximize then (max alpha z, beta) else (alpha, min beta z)
           in if beta' <= alpha' then best' else searchAlphaBetaGeneric'' f xs alpha' beta' maximize sofar (Just best')
        else searchAlphaBetaGeneric'' f xs alpha beta maximize sofar best
searchAlphaBetaGeneric'' _ [] _ _ _ _ (Just best) = best -- examined all candidates
searchAlphaBetaGeneric'' _ [] _ _ _ _ _ = undefined -- never happens
