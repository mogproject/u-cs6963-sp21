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

import Data.Bits (complement, popCount, shift, (.&.))
import Data.Map (Map, (!))
import qualified Data.Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as V
import Game.GameMove
import Game.GameState
  ( AdjList,
    Bitmap,
    GameState (GameState),
    Index,
    Levels,
    Players,
    buildAdjacency,
    fromList,
    legalMoves,
    levelMap,
    levels,
    moveAdjacency,
    players,
    toList,
    turn,
  )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Score = Int

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
        [scoreWin, 20000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0], -- level 4

        -- not turn to move
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [300, 100, 40, 30, 20, 15, 12, 0], -- level 1
        [10000, 1000, 500, 200, 150, 120, 110, 0], -- level 2
        [scoreWin, 10000, 5000, 2000, 1000, 500, 300, 0], -- level 3
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

--------------------------------------------------------------------------------
-- Distance Computation
--------------------------------------------------------------------------------

distInf :: Int
distInf = 100 -- infinity distance

-- breadth-first search
bfs :: AdjList -> Bitmap -> Index -> Map Index Int
bfs adj avail start =
  let initMap = Data.Map.fromList [(i, if i == start then 0 else distInf) | i <- [0 .. 24]]
   in bfs' adj avail (Seq.singleton start) initMap

bfs' :: AdjList -> Bitmap -> Seq Index -> Map Index Int -> Map Index Int
bfs' _ _ q sofar | Seq.null q = sofar
bfs' adj avail q sofar =
  let x = q `Seq.index` 0
      nbrs = toList $ (adj ! x) .&. avail
      unseen = Seq.fromList [nbr | nbr <- nbrs, (sofar ! nbr) == distInf]
      d = (sofar ! x) + 1
      q' = Seq.drop 1 q Seq.>< unseen
      sofar' = foldl (\m u -> Data.Map.insert u d m) sofar unseen
   in bfs' adj avail q' sofar'

getDistances :: Players -> AdjList -> ([[Map Index Int]], [V.Vector Int])
getDistances pl adjM =
  let ps = [fromList ws | ws <- pl] -- player bitmaps
      dist = [[bfs adjM (complement (ps !! (1 - p))) (pl !! p !! w) | w <- [0, 1]] | p <- [0, 1]]
      bestDist = [V.fromList [minimum [dist !! p !! w ! i | w <- [0, 1]] | i <- [0 .. 24]] | p <- [0, 1]]
   in (dist, bestDist)

--------------------------------------------------------------------------------
-- Logic
--------------------------------------------------------------------------------
-- TODO: halve the distance if the card is Artemis
evaluate :: GameState -> Score
evaluate
  g@GameState
    { players = pl,
      levels = lv,
      turn = _,
      levelMap = _,
      moveAdjacency = adjM,
      buildAdjacency = adjB,
      legalMoves = _
    } = case evaluate' g of
    (Just sc, _) -> sc
    (Nothing, _) ->
      let (dist, bestDist) = getDistances pl adjM
          funcs =
            [ evaluateWorkerProximity pl dist,
              evaluateReachability lv bestDist,
              evaluateAsymmetry pl lv adjB bestDist,
              evaluateStuckBonus pl adjM,
              evaluatePrevention pl lv adjB bestDist
            ]
       in sum [s * f p | (s, p) <- [(1, 0), (-1, 1)], f <- funcs]

evaluate' :: GameState -> (Maybe Score, Maybe GameMove)
evaluate'
  GameState
    { players = pl,
      levels = lv,
      turn = _,
      levelMap = lm,
      moveAdjacency = adjM,
      buildAdjacency = _,
      legalMoves = mv
    }
    | opponentWin = (Just (- scoreWin), Nothing)
    | canWin = (Just scoreWin, Just (head [m | m <- mv, let mt = getMoveTo m, lv ! mt == 3]))
    | otherwise = (Nothing, Nothing)
    where
      -- FIXME: when cards are introduced
      opponentWin = null mv || elem 3 [lv ! i | i <- pl !! 1]
      canWin = any (\i -> (adjM ! i) .&. (lm ! 3) /= 0) (pl !! 0)
      -- TODO: implement
      canOpponentWin = undefined

-- (1) Worker Proximity
evaluateWorkerProximity :: Players -> [[Map Index Int]] -> Int -> Score
evaluateWorkerProximity pl dist p = sum [getProxTableValue (dist !! p !! w ! (pl !! p !! (1 - w))) | w <- [0, 1]]

-- (2) Reachability
evaluateReachability :: Levels -> [V.Vector Int] -> Int -> Score
evaluateReachability lv dist p = sum [getReachTableValue p (lv ! i) (dist !! p V.! i) | i <- [0 .. 24]]

-- (3) Asymmetry
evaluateAsymmetry :: Players -> Levels -> AdjList -> [V.Vector Int] -> Int -> Score
evaluateAsymmetry pl lv adj dist p = sum [g i | i <- [0 .. 24]]
  where
    ws = sum [(1 :: Int) `shift` x | x <- concat pl] -- all workers
    cornerBonus i = (8 - popCount (adj ! i .&. complement ws)) * (lv ! i) * evalCornerBonus
    g i =
      let diff = (dist !! (1 - p) V.! i) - (dist !! p V.! i)
       in if diff > 0 then cornerBonus i + getAsymTableValue (lv ! i) diff else 0

-- (4) Stuck Bonus
evaluateStuckBonus :: Players -> AdjList -> Int -> Score
evaluateStuckBonus pl adj p = sum [if adj ! (pl !! (1 - p) !! w) == 0 then evalStuckBonus else 0 | w <- [0, 1]]

-- (5) Prevension: worker at height 2, opponent cannot approach
evaluatePrevention :: Players -> Levels -> AdjList -> [V.Vector Int] -> Int -> Score
evaluatePrevention pl lv adj dist p = sum [f index | w <- [0, 1], let index = pl !! p !! w, lv ! index == 2]
  where
    f index =
      let nbrs = adj ! index
       in if nbrs > 0 && minimum [dist !! (1 - p) V.! u | u <- toList nbrs] >= 2
            then evalPreventionAdvantage
            else 0

--------------------------------------------------------------------------------
-- For unit testing
--------------------------------------------------------------------------------

evaluateWorkerProximity' :: GameState -> Int -> Score
evaluateWorkerProximity' GameState {players = pl, moveAdjacency = adjM} = evaluateWorkerProximity pl (fst $ getDistances pl adjM)

evaluateReachability' :: GameState -> Int -> Score
evaluateReachability' GameState {players = pl, levels = lv, moveAdjacency = adjM} = evaluateReachability lv (snd $ getDistances pl adjM)

evaluateAsymmetry' :: GameState -> Int -> Score
evaluateAsymmetry' GameState {players = pl, levels = lv, moveAdjacency = adjM, buildAdjacency = adjB} = evaluateAsymmetry pl lv adjB (snd $ getDistances pl adjM)

evaluateStuckBonus' :: GameState -> Int -> Score
evaluateStuckBonus' GameState {players = pl, moveAdjacency = adjM} = evaluateStuckBonus pl adjM

evaluatePrevention' :: GameState -> Int -> Score
evaluatePrevention' GameState {players = pl, levels = lv, moveAdjacency = adjM, buildAdjacency = adjB} = evaluatePrevention pl lv adjB (snd $ getDistances pl adjM)
