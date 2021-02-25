module Search.Search (findMove) where

-- import Data.Function (on)
import Data.List (maximumBy, minimumBy, sortBy)
-- import Data.Map ((!))
import Data.Ord (comparing)
import Game.Evaluation (Score, evaluate, evaluate', scoreWin)
import Game.GameState (GameMove, GameState (GameState), makeMove)
import qualified Game.GameState as GS

findMove :: Int -> Int -> GameState -> GameMove
-- Naive: choose the first legal move
findMove strategy _ GameState {GS.legalMoves = mv} | strategy == 1 && not (null mv) = head mv
-- Minimax
findMove strategy _ g@GameState {GS.legalMoves = mv} | strategy == 2 && not (null mv) = searchMiniMax g 2
-- Alpha-beta
findMove strategy _ g@GameState {GS.legalMoves = mv} | strategy == 3 && not (null mv) = searchAlphaBetaNaive g 3
-- Alpha-beta with reordering
findMove strategy _ g@GameState {GS.legalMoves = mv} | strategy == 4 && not (null mv) = searchAlphaBetaReordering g 3
findMove _ _ _ = undefined

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

searchAlphaBetaNaive :: GameState -> Int -> GameMove
searchAlphaBetaNaive g depth = last . snd $ searchAlphaBetaNaive' g depth (- scoreWin) scoreWin True []

searchAlphaBetaNaive' :: GameState -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove])
-- reached depth limit
searchAlphaBetaNaive' g depth _ _ shouldMaximize sofar
  | depth == 0 =
    ((if shouldMaximize then 1 else -1) * evaluate g, sofar)
--
searchAlphaBetaNaive' g@GameState {GS.legalMoves = mv} depth alpha beta shouldMaximize sofar =
  case evaluate' g of
    (Just sc, Nothing) -> ((if shouldMaximize then 1 else -1) * sc, sofar) -- terminal node
    (Just sc, Just m) -> ((if shouldMaximize then 1 else -1) * sc, m : sofar) -- terminal node
    (Nothing, _) ->
      let nextStates = fmap (makeMove g) mv
          bestScore = if shouldMaximize then (- scoreWin - 1) else (scoreWin + 1)
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

-- Alpha-beta with reordering heuristic
searchAlphaBetaReordering :: GameState -> Int -> GameMove
searchAlphaBetaReordering g depth = last . snd $ searchAlphaBetaReordering' g depth (- scoreWin) scoreWin True []

searchAlphaBetaReordering' :: GameState -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove])
-- reached depth limit
searchAlphaBetaReordering' g depth _ _ shouldMaximize sofar
  | depth == 0 =
    ((if shouldMaximize then 1 else -1) * evaluate g, sofar)
--
searchAlphaBetaReordering' g@GameState {GS.legalMoves = mv} depth alpha beta shouldMaximize sofar =
  case evaluate' g of
    (Just sc, Nothing) -> ((if shouldMaximize then 1 else -1) * sc, sofar) -- terminal node
    (Just sc, Just m) -> ((if shouldMaximize then 1 else -1) * sc, m : sofar) -- terminal node
    (Nothing, _) ->
      -- FIXME: cache evaluation results
      let nextStates = sortBy (comparing ((if shouldMaximize then negate else id) . fst)) [(evaluate st, (m, st)) | m <- mv, let st = makeMove g m]
          bestScore = if shouldMaximize then (- scoreWin - 1) else (scoreWin + 1)
       in searchAlphaBetaReordering'' (map snd nextStates) depth alpha beta shouldMaximize sofar (bestScore, [])

searchAlphaBetaReordering'' :: [(GameMove, GameState)] -> Int -> Score -> Score -> Bool -> [GameMove] -> (Score, [GameMove]) -> (Score, [GameMove])
searchAlphaBetaReordering'' [] _ _ _ _ _ best = best
searchAlphaBetaReordering'' ((mv, st) : gs) depth alpha beta shouldMaximize sofar (bestScore, bestMove) =
  let (value, moves) = searchAlphaBetaReordering' st (depth -1) alpha beta (not shouldMaximize) (mv : sofar)
   in if (shouldMaximize && value > bestScore) || (not shouldMaximize && value < bestScore)
        then do
          let best' = (value, moves)
          let alpha' = if shouldMaximize then max alpha value else alpha
          let beta' = if shouldMaximize then beta else min beta value
          if beta' <= alpha'
            then best' -- cutoff
            else searchAlphaBetaReordering'' gs depth alpha' beta' shouldMaximize sofar best'
        else searchAlphaBetaReordering'' gs depth alpha beta shouldMaximize sofar (bestScore, bestMove)
