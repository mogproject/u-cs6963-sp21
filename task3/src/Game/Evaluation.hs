module Game.Evaluation
  ( Score,
    evaluate,
    evaluate',
    scoreWin,
    evaluateWorkerProximity',
    evaluateReachability',
    evaluateAsymmetry',
    evaluateStuckBonus',
    evaluatePrevention',
  )
where

import Data.Bits (complement, (.&.), (.|.))
import Data.Card (Card (Apollo, Artemis, Atlas, Demeter, Hephastus, Minotaur, Pan, Prometheus))
import Data.Map (Map, (!))
import qualified Data.Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as V
import Game.BitBoard
import Game.GameMove
import Game.GameState

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Score = Int

type AdjList = V.Vector BitBoard

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

scoreWin :: Score
scoreWin = 1000000000 -- 1e9

evalProxTable :: V.Vector Score
evalProxTable = V.fromList [0, 100, 80, 50, 20, 0]

evalProxTableSize :: Int
evalProxTableSize = V.length evalProxTable

getProxTableValue :: Int -> Score
getProxTableValue dist = evalProxTable V.! dist'
  where
    dist' = min (evalProxTableSize - 1) dist

evalReachTable :: V.Vector Score
evalReachTable =
  V.fromList $
    concat
      [ -- turn to move (advantageous)
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [300, 200, 40, 30, 20, 15, 12, 0], -- level 1
        [10000, 2000, 500, 200, 150, 120, 110, 0], -- level 2
        [0, 20000, 5000, 2000, 1000, 500, 300, 0], -- level 3 (staying at lv 3 is not advantageous)
        [0, 0, 0, 0, 0, 0, 0, 0], -- level 4

        -- not turn to move
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [300, 100, 40, 30, 20, 15, 12, 0], -- level 1
        [10000, 1000, 500, 200, 150, 120, 110, 0], -- level 2
        [0, 10000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0] -- level 4
      ]

evalReachTableSize :: Int
evalReachTableSize = V.length evalReachTable `div` 10

getReachTableValue :: Int -> Int -> Int -> Score
getReachTableValue player lev dist = evalReachTable V.! (player * evalReachTableSize * 5 + lev * evalReachTableSize + dist')
  where
    dist' = min (evalReachTableSize - 1) dist

evalAsymTable :: V.Vector Score
evalAsymTable =
  V.fromList $
    concat
      [ [0, 0, 0, 1, 2, 3, 4, 5, 10], -- level 0
        [0, 0, 0, 10, 20, 30, 40, 50, 100], -- level 1
        [0, 100, 1000, 2000, 2000, 2000, 2000, 2000, 2000], -- level 2
        [0, 1000, 20000, 30000, 30000, 30000, 30000, 30000, 30000], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0, 0] -- level 4
      ]

evalAsymTableSize :: Int
evalAsymTableSize = V.length evalAsymTable `div` 5

getAsymTableValue :: Int -> Int -> Score
getAsymTableValue lev dist = evalAsymTable V.! (lev * evalAsymTableSize + dist')
  where
    dist' = min (evalAsymTableSize - 1) dist

evalCornerBonus :: Score
evalCornerBonus = 20

evalStuckBonus :: Score
evalStuckBonus = 25000

evalPreventionAdvantage :: Score
evalPreventionAdvantage = 20000

evalPanBonus :: Score
evalPanBonus = 25000

--------------------------------------------------------------------------------
-- Distance Computation
--------------------------------------------------------------------------------

distInf :: Int
distInf = 100 -- infinity distance

-- breadth-first search
bfs :: AdjList -> BitBoard -> Index -> Map Index Int
bfs adj avail start =
  let initMap = Data.Map.fromList [(i, if i == start then 0 else distInf) | i <- validIndices]
   in bfs' adj avail (Seq.singleton start) initMap

bfs' :: AdjList -> BitBoard -> Seq Index -> Map Index Int -> Map Index Int
bfs' _ _ q sofar | Seq.null q = sofar
bfs' adj avail q sofar =
  let x = q `Seq.index` 0
      nbrs = bbToList $ (adj V.! x) .&. avail
      unseen = Seq.fromList [nbr | nbr <- nbrs, (sofar ! nbr) == distInf]
      d = (sofar ! x) + 1
      q' = Seq.drop 1 q Seq.>< unseen
      sofar' = foldl (\m u -> Data.Map.insert u d m) sofar unseen
   in bfs' adj avail q' sofar'

getDistances :: Cards -> Players -> AdjList -> ([[Map Index Int]], [Map Index Int])
getDistances cs pl adj =
  let ps = [listToBB ws | ws <- pl] -- player bitboards
      f p = any (\x -> x `elem` (cs !! p)) [Apollo, Minotaur] -- those gods ignore opponent's positions
      obstacles = [if f p then 0 else ps !! (1 - p) | p <- [0, 1]]
      dist' = [[bfs adj (complement (obstacles !! p)) (pl !! p !! w) | w <- [0, 1]] | p <- [0, 1]]
      dist = [if Artemis `elem` (cs !! p) then [Data.Map.map (\x -> (x + 1) `div` 2) (dist' !! p !! w) | w <- [0, 1]] else dist' !! p | p <- [0, 1]]
      bestDist = [Data.Map.fromList [(i, minimum [dist !! p !! w ! i | w <- [0, 1]]) | i <- validIndices] | p <- [0, 1]]
   in (dist, bestDist)

--------------------------------------------------------------------------------
-- Logic
--------------------------------------------------------------------------------
createAdjacencyList :: Levels -> LevelMap -> AdjList
createAdjacencyList lv lm = V.fromList [if isValidIndex i then getLegalMoveTo' i lv lm else 0 | i <- [0 .. (last validIndices)]]

evaluate :: GameState -> Score
evaluate
  g@GameState
    { cards = cs,
      players = pl,
      playerMap = _,
      levels = lv,
      levelMap = lm,
      turn = t,
      legalMoves = _
    } = case evaluate' g of
    (Just sc, _) -> sc
    (Nothing, _) ->
      let adj = V.fromList [if isValidIndex i then getLegalMoveTo' i lv lm else 0 | i <- [0 .. (last validIndices)]]
          (dist, bestDist) = getDistances cs pl adj
          funcs =
            [ evaluateWorkerProximity pl dist,
              evaluateReachability lv bestDist,
              evaluateAsymmetry pl lv lm bestDist,
              evaluateStuckBonus pl adj,
              evaluatePrevention pl lv lm bestDist,
              evaluatePanBonus cs pl lv
            ]
       in sign * sum [s * f p | (s, p) <- [(1, 0), (-1, 1)], f <- funcs]
    where
      sign = if even t then 1 else -1

evaluate' :: GameState -> (Maybe Score, Maybe GameMove)
evaluate'
  GameState {turn = t, legalMoves = mv}
    | hasLost = (Just (sign * (- scoreWin)), Nothing)
    | canWin = (Just (sign * scoreWin), Just (head mv)) -- moves must be sorted
    | otherwise = (Nothing, Nothing)
    where
      hasLost = null mv
      canWin = getWin $ head mv
      sign = if even t then 1 else -1

-- (1) Worker Proximity
evaluateWorkerProximity :: Players -> [[Map Index Int]] -> Int -> Score
evaluateWorkerProximity pl dist p = sum [getProxTableValue (dist !! p !! w ! (pl !! p !! (1 - w))) | w <- [0, 1]]

-- (2) Reachability
evaluateReachability :: Levels -> [Map Index Int] -> Int -> Score
evaluateReachability lv dist p = sum [getReachTableValue p (lv ! i) (dist !! p ! i) | i <- validIndices]

-- (3) Asymmetry
evaluateAsymmetry :: Players -> Levels -> LevelMap -> [Map Index Int] -> Int -> Score
evaluateAsymmetry pl lv lm dist p = sum . map g $ validIndices
  where
    ws = sum . map singletonBB . concat $ pl -- all workers
    cornerBonus i = (8 - countBB (getNeighborhood i `andNotBB` (ws .|. (lm ! 4)))) * (lv ! i) * evalCornerBonus
    g i =
      let diff = (dist !! (1 - p) ! i) - (dist !! p ! i)
       in if diff > 0 then cornerBonus i + getAsymTableValue (lv ! i) diff else 0

-- (4) Stuck Bonus
evaluateStuckBonus :: Players -> AdjList -> Int -> Score
evaluateStuckBonus pl adj p = sum [if adj V.! (pl !! (1 - p) !! w) == 0 then evalStuckBonus else 0 | w <- [0, 1]]

-- (5) Prevension: worker at height 2, opponent cannot approach
evaluatePrevention :: Players -> Levels -> LevelMap -> [Map Index Int] -> Int -> Score
evaluatePrevention pl lv lm dist p = sum [f index | w <- [0, 1], let index = pl !! p !! w, lv ! index == 2]
  where
    f index =
      let nbrs = getNeighborhood index `andNotBB` (lm ! 4)
       in if nbrs > 0 && minimum [dist !! (1 - p) ! u | u <- bbToList nbrs] >= 2
            then evalPreventionAdvantage
            else 0

-- (6) Pan's Level 2 Bonus
evaluatePanBonus :: Cards -> Players -> Levels -> Int -> Score
evaluatePanBonus cs pl lv p =
  if Pan `elem` (cs !! p)
    then sum [if lv ! w == 2 then evalPanBonus else 0 | w <- pl !! p]
    else 0

--------------------------------------------------------------------------------
-- For unit testing
--------------------------------------------------------------------------------

evaluateWorkerProximity' :: GameState -> Int -> Score
evaluateWorkerProximity' GameState {cards = cs, players = pl, levels = lv, levelMap = lm} = evaluateWorkerProximity pl (fst $ getDistances cs pl (createAdjacencyList lv lm))

evaluateReachability' :: GameState -> Int -> Score
evaluateReachability' GameState {cards = cs, players = pl, levels = lv, levelMap = lm} = evaluateReachability lv (snd $ getDistances cs pl (createAdjacencyList lv lm))

evaluateAsymmetry' :: GameState -> Int -> Score
evaluateAsymmetry' GameState {cards = cs, players = pl, levels = lv, levelMap = lm} = evaluateAsymmetry pl lv lm (snd $ getDistances cs pl (createAdjacencyList lv lm))

evaluateStuckBonus' :: GameState -> Int -> Score
evaluateStuckBonus' GameState {players = pl, levels = lv, levelMap = lm} = evaluateStuckBonus pl (createAdjacencyList lv lm)

evaluatePrevention' :: GameState -> Int -> Score
evaluatePrevention' GameState {cards = cs, players = pl, levels = lv, levelMap = lm} = evaluatePrevention pl lv lm (snd $ getDistances cs pl (createAdjacencyList lv lm))
