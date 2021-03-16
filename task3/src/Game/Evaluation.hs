{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Game.Evaluation
  ( Score,
    evaluate,
    evaluate',
    evaluateDetails,
    scoreWin,
    evaluateWorkerProximity',
    evaluateReachability',
    evaluateAsymmetry',
    evaluateStuckBonus',
    evaluatePrevention',
    hasDoubleLizhi,
    hasOneXiangting,
    hasOneXiangtingPrometheus',
    hasOneXiangtingMinotaur'',
    hasOneXiangtingArtemis,
    bfsBB,
    bfsBB',
  )
where

import Data.Bits (Bits (xor), complement, (.&.), (.|.))
import Data.Card (Card (Apollo, Artemis, Demeter, Hephastus, Minotaur, Pan, Prometheus))
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as Map
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
evalProxTable = V.fromList [0, 1000, 990, 50, 20, 0]

evalProxDisconnected :: Score
evalProxDisconnected = -20000

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
        [30000, 2000, 500, 200, 150, 120, 110, 0], -- level 2
        [0, 20000, 5000, 2000, 1000, 500, 300, 0], -- level 3 (staying at lv 3 is not advantageous)
        [0, 0, 0, 0, 0, 0, 0, 0], -- level 4

        -- not turn to move
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [300, 100, 40, 30, 20, 15, 12, 0], -- level 1
        [30000, 1000, 500, 200, 150, 120, 110, 0], -- level 2
        [0, 10000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0] -- level 4
      ]

evalReachTableForPan :: V.Vector Score
evalReachTableForPan =
  V.fromList $
    concat
      [ -- turn to move (advantageous)
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [10000, 200, 40, 30, 20, 15, 12, 0], -- level 1
        [30000, 12000, 500, 200, 150, 120, 110, 0], -- level 2 (bias towards moving up to lv 2)
        [30000, 20000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0], -- level 4

        -- not turn to move
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [10000, 100, 40, 30, 20, 15, 12, 0], -- level 1
        [30000, 11000, 500, 200, 150, 120, 110, 0], -- level 2
        [30000, 20000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0] -- level 4
      ]

evalReachTableSize :: Int
evalReachTableSize = V.length evalReachTable `div` 10

getReachTableValue :: Maybe Card -> Int -> Int -> Int -> Score
getReachTableValue (Just Pan) player lev dist = evalReachTableForPan V.! (player * evalReachTableSize * 5 + lev * evalReachTableSize + dist')
  where
    dist' = min (evalReachTableSize - 1) dist
getReachTableValue _ player lev dist = evalReachTable V.! (player * evalReachTableSize * 5 + lev * evalReachTableSize + dist')
  where
    dist' = min (evalReachTableSize - 1) dist

evalAsymTable :: V.Vector Score
evalAsymTable =
  V.fromList $
    concat
      [ [0, 0, 0, 1, 2, 3, 4, 5, 10], -- level 0
        [0, 0, 0, 10, 20, 30, 40, 50, 9000], -- level 1
        [0, 100, 1000, 2000, 2000, 2000, 2000, 2000, 10000], -- level 2
        [0, 0, 0, 0, 0, 0, 0, 0, 0], -- level 3 (not very important as this leads to endgame)
        -- [0, 1000, 20000, 30000, 30000, 30000, 30000, 30000, 30000], -- level 3
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

evalPrometeus312 :: Score
evalPrometeus312 = 40000

evalPrometeus322 :: Score
evalPrometeus322 = 70000

evalDoubleLizhi :: Score
evalDoubleLizhi = 10000000

-- one step away from double lizhi
evalOneXiangting :: Score
evalOneXiangting = 1000000

evalStepping01 :: Score
evalStepping01 = 1000

evalStepping11 :: Score
evalStepping11 = 10000

evalStepping12 :: Score
evalStepping12 = 40000

evalStepping22 :: Score
evalStepping22 = 60000

--------------------------------------------------------------------------------
-- Distance Computation
--------------------------------------------------------------------------------

-- does not distinguish winning moves
getLegalMoveToBB_ :: LevelMap -> [BitBoard] -> [BitBoard]
getLegalMoveToBB_ lm [x0, x1, x2, x3] =
  let xx = x0 .|. x1 .|. x2 .|. x3
      y0 = getClosedNeighborhood xx .&. (lm !! 0)
      y1 = getClosedNeighborhood xx .&. (lm !! 1)
      y2 = getClosedNeighborhood (xx `xor` x0) .&. (lm !! 2)
      y3 = getClosedNeighborhood (x2 .|. x3) .&. (lm !! 3)
   in [y0, y1, y2, y3]
getLegalMoveToBB_ _ _ = undefined

-- getLegalMvoeToBB :: Index -> PlayerMap -> LevelMap -> BitBoard
-- getLegalMvoeToBB moveFrom pm lm =

bfsBB :: Cards -> Int -> Index -> PlayerMap -> LevelMap -> [BitBoard]
bfsBB cs p moveFrom pm lm =
  let forbidden = if (cs !! p) `elem` [Just Apollo, Just Minotaur] then 0 else pm !! (5 - p)
   in bfsBB' forbidden moveFrom lm

bfsBB' :: BitBoard -> Int -> LevelMap -> [BitBoard]
bfsBB' forbidden moveFrom lm =
  let mf = singletonBB moveFrom
      avail = mf .|. (complement forbidden)
      lm' = (map (.&. avail) . take 4) lm
   in bfsBB'' mf lm'

bfsBB'' :: BitBoard -> LevelMap -> [BitBoard]
bfsBB'' mf lm = (takeWhile (/= 0) . map (sum . fst)) $ iterate f (map (.&. mf) lm, mf)
  where
    f (frontier, visited) =
      let ys = getLegalMoveToBB_ lm frontier
       in (map (`andNotBB` visited) ys, visited .|. (sum ys))

distInf :: Int
distInf = 100 -- infinity distance

-- breadth-first search
bfs :: AdjList -> BitBoard -> Index -> IntMap Int
bfs adj avail start =
  let initMap = Map.fromList [(i, if i == start then 0 else distInf) | i <- validIndices]
   in bfs' adj avail (Seq.singleton start) initMap

bfs' :: AdjList -> BitBoard -> Seq Index -> IntMap Int -> IntMap Int
bfs' _ _ q sofar | Seq.null q = sofar
bfs' adj avail q sofar =
  let x = q `Seq.index` 0
      nbrs = bbToList $ (adj V.! x) .&. avail
      unseen = Seq.fromList [nbr | nbr <- nbrs, (sofar ! nbr) == distInf]
      d = (sofar ! x) + 1
      q' = Seq.drop 1 q Seq.>< unseen
      sofar' = foldl (\m u -> Map.insert u d m) sofar unseen
   in bfs' adj avail q' sofar'

getDistances :: Cards -> Players -> PlayerMap -> AdjList -> ([[IntMap Int]], [IntMap Int])
getDistances cs pl pm adj =
  let obstacles = [if (cs !! p) `elem` [Just Apollo, Just Minotaur] then 0 else pm !! (5 - p) | p <- [0, 1]]
      dist' = [[bfs adj (complement (obstacles !! p)) (pl !! p !! w) | w <- [0, 1]] | p <- [0, 1]]
      dist = [if Artemis `elem` (cs !! p) then [Map.map (\x -> (x + 1) `div` 2) (dist' !! p !! w) | w <- [0, 1]] else dist' !! p | p <- [0, 1]]
      bestDist = [Map.fromList [(i, minimum [dist !! p !! w ! i | w <- [0, 1]]) | i <- validIndices] | p <- [0, 1]]
   in (dist, bestDist)

--------------------------------------------------------------------------------
-- Logic
--------------------------------------------------------------------------------
createAdjacencyList :: LevelMap -> AdjList
createAdjacencyList lm = V.fromList [if isValidIndex i then getLegalMoveTo' i lm else 0 | i <- [0 .. (last validIndices)]]

evaluate :: GameState -> Score
evaluate g = case evaluate' g of
  (Just sc, _) -> sc
  (Nothing, _) -> (sum . concat) (evaluateDetails g)

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

evaluateDetails :: GameState -> [[Score]]
evaluateDetails
  GameState
    { cards = cs,
      players = pl,
      playerMap = pm,
      levels = lv,
      levelMap = lm,
      turn = t,
      legalMoves = _
    } =
    let adj = V.fromList [if isValidIndex i then getLegalMoveTo' i lm else 0 | i <- [0 .. (last validIndices)]]
        (dist, bestDist) = getDistances cs pl pm adj
        funcs =
          [ evaluateWorkerProximity pl dist,
            evaluateReachability cs lv bestDist,
            evaluateAsymmetry pl lv lm bestDist,
            evaluateStuckBonus pl adj,
            evaluatePrevention pl lv lm bestDist,
            evaluateDoubleLizhiBonus cs pl pm lv lm,
            evaluatePrometeusBonus cs pm lm,
            evaluateStepping cs pl pm lv lm bestDist
          ]
     in [[(if even (t + p) then 1 else -1) * f p | f <- funcs] | p <- [0, 1]]

-- (1) Worker Proximity
evaluateWorkerProximity :: Players -> [[IntMap Int]] -> Int -> Score
evaluateWorkerProximity pl dist p =
  let val = sum [getProxTableValue (dist !! p !! w ! (pl !! p !! (1 - w))) | w <- [0, 1]]
   in if val == 0 then evalProxDisconnected else val

-- (2) Reachability
evaluateReachability :: Cards -> Levels -> [IntMap Int] -> Int -> Score
evaluateReachability cs lv dist p = sum [getReachTableValue (cs !! p) p (lv ! i) (dist !! p ! i) | i <- validIndices]

-- (3) Asymmetry
evaluateAsymmetry :: Players -> Levels -> LevelMap -> [IntMap Int] -> Int -> Score
evaluateAsymmetry pl lv lm dist p = sum . map g $ validIndices
  where
    ws = sum . map singletonBB . concat $ pl -- all workers
    cornerBonus i = (8 - countBB (getNeighborhood i `andNotBB` (ws .|. (lm !! 4)))) * (lv ! i) * evalCornerBonus
    g i =
      let diff = (dist !! (1 - p) ! i) - (dist !! p ! i)
       in if diff > 0 then cornerBonus i + getAsymTableValue (lv ! i) diff else 0

-- (4) Stuck Bonus
evaluateStuckBonus :: Players -> AdjList -> Int -> Score
evaluateStuckBonus pl adj p = sum [if adj V.! (pl !! (1 - p) !! w) == 0 then evalStuckBonus else 0 | w <- [0, 1]]

-- (5) Prevension: worker at height 2, opponent cannot approach
evaluatePrevention :: Players -> Levels -> LevelMap -> [IntMap Int] -> Int -> Score
evaluatePrevention pl lv lm dist p = sum [f index | w <- [0, 1], let index = pl !! p !! w, lv ! index == 2]
  where
    f index =
      let nbrs = getNeighborhood index `andNotBB` (lm !! 4)
       in if nbrs > 0 && minimum [dist !! (1 - p) ! u | u <- bbToList nbrs] >= 2
            then evalPreventionAdvantage
            else 0

-- (6) Double Lizhi Bonus
evaluateDoubleLizhiBonus :: Cards -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Score
evaluateDoubleLizhiBonus cs pl pm lv lm p
  | hasDoubleLizhi (cs !! p) p pm lm = evalDoubleLizhi
  | hasOneXiangting (cs !! p) p pl pm lv lm = evalOneXiangting
  | otherwise = 0

hasDoubleLizhi :: Maybe Card -> Int -> PlayerMap -> LevelMap -> Bool
-- Minotaur
hasDoubleLizhi (Just Minotaur) p pm lm =
  let ours = pm !! (4 + p)
      nbrs = getClosedNeighborhood (ours .&. (lm !! 2)) .&. (lm !! 3)
      opponentNbrs = nbrs .&. (pm !! (5 - p))
      emptyNbrs = nbrs `andNotBB` (pm !! 6)
      emptyCnt = countBB emptyNbrs
      pushableNbrCnt =
        length $
          filter
            (\i -> (getPushBB ours (singletonBB i) .&. (lm !! 7) `andNotBB` (pm !! 6)) /= 0)
            $ bbToList opponentNbrs
   in emptyCnt >= 2 || emptyCnt + pushableNbrCnt >= 2
-- Artemis
hasDoubleLizhi (Just Artemis) p pm lm =
  let lv2 = ((pm !! (4 + p)) .|. (getClosedNeighborhood (pm !! (4 + p) `andNotBB` (lm !! 0)) `andNotBB` (pm !! 6))) .&. (lm !! 2)
   in countBB (getClosedNeighborhood lv2 .&. (lm !! 3) `andNotBB` (pm !! 6)) >= 2
-- Pan
hasDoubleLizhi (Just Pan) p pm lm =
  let fromLv2 = getClosedNeighborhood ((pm !! (4 + p)) .&. (lm !! 2)) `andNotBB` (pm !! 6)
      fromLv3 = getClosedNeighborhood ((pm !! (4 + p)) .&. (lm !! 3)) `andNotBB` (pm !! 6)
   in countBB ((fromLv2 .&. ((lm !! 0) .|. (lm !! 3))) .|. (fromLv3 .&. (lm !! 5))) >= 2
-- others
hasDoubleLizhi _ p pm lm =
  let emptyNbrs = getClosedNeighborhood ((pm !! (4 + p)) .&. (lm !! 2)) `andNotBB` (pm !! 6)
   in countBB (emptyNbrs .&. (lm !! 3)) >= 2

hasOneXiangting :: Maybe Card -> Int -> Players -> PlayerMap -> Levels -> LevelMap -> Bool
-- Apollo
hasOneXiangting c@(Just Apollo) p pl pm lv lm =
  let ours = pm !! (4 + p)
      moveFrom = ours `andNotBB` (lm !! 0)
      moveTo = getClosedNeighborhood moveFrom .&. (lm !! 2) `andNotBB` (pm !! (4 + p)) .&. getClosedNeighborhood (lm !! 3)
   in canCooperatePattern1 c pl pm lv lm p || any (canMakeLizhi c pm lm p) (bbToList moveTo)
-- Artemis
hasOneXiangting (Just Artemis) p pl pm lv lm = any (hasOneXiangtingArtemis p pl pm lv lm) [0, 1]
-- Minotaur
hasOneXiangting (Just Minotaur) p pl pm lv lm = any (hasOneXiangtingMinotaur p pl pm lv lm) [0, 1]
-- Prometheus
hasOneXiangting (Just Prometheus) p pl pm lv lm = any (hasOneXiangtingPrometheus p pl pm lv lm) [0, 1]
-- Demeter
hasOneXiangting c@(Just Demeter) p pl pm lv lm =
  let ours = pm !! (4 + p)
      moveFrom = ours `andNotBB` (lm !! 0)
      moveTo = getClosedNeighborhood moveFrom .&. (lm !! 2) `andNotBB` (pm !! 6)
   in canCooperatePattern1 c pl pm lv lm p || canCooperatePattern2 Demeter pl pm lv lm p || any (canMakeLizhi c pm lm p) (bbToList moveTo)
-- Pan
hasOneXiangting (Just Pan) p _ pm _ lm =
  let ours = pm !! (4 + p)
      oursLv2 = ours .&. (lm !! 2)
      oursLv2Nbr = getClosedNeighborhood oursLv2
      oursLv0 = ours .&. (lm !! 0)
      oursLv3 = ours .&. (lm !! 3)
      oursLv03 = oursLv0 .|. oursLv3
      -- lv2 and lv0/3 workers are adjacent
      considerCooperate = oursLv2Nbr .&. oursLv03 /= 0
      oursLv03MoveTo =
        ( (getClosedNeighborhood oursLv0 .&. (lm !! 5))
            .|. (getClosedNeighborhood oursLv3 .&. (lm !! 7))
        )
          `andNotBB` (pm !! 6)
      oursLv03TargetBuildAt = oursLv2Nbr .&. (lm !! 2) `andNotBB` (pm !! 6)
      canCoorporate = considerCooperate && (getOpenNeighborhood oursLv03TargetBuildAt .&. oursLv03MoveTo) /= 0
      opponent = pm !! (5 - p)
      moveFrom = ours `andNotBB` (lm !! 0)
      -- there's no path for Pan from lv 0 to lv 2
      nextMoveCandidates = ((lm !! 0) `andNotBB` (pm !! 6)) .|. ((lm !! 3) `andNotBB` opponent)
      moveTo = getClosedNeighborhood moveFrom .&. (lm !! 2) `andNotBB` (pm !! 6) .&. getClosedNeighborhood nextMoveCandidates
   in canCoorporate || any (canMakeLizhi (Just Pan) pm lm p) (bbToList moveTo)
-- Hephastus
hasOneXiangting c@(Just Hephastus) p pl pm lv lm =
  let ours = pm !! (4 + p)
      moveFrom = ours `andNotBB` (lm !! 0)
      moveTo = getClosedNeighborhood moveFrom .&. (lm !! 2) `andNotBB` (pm !! 6) .&. getClosedNeighborhood (lm !! 3)
   in canCooperatePattern1 c pl pm lv lm p || any (canMakeLizhi c pm lm p) (bbToList moveTo)
-- others
hasOneXiangting c p pl pm lv lm =
  let ours = pm !! (4 + p)
      moveFrom = ours `andNotBB` (lm !! 0)
      moveTo = getClosedNeighborhood moveFrom .&. (lm !! 2) `andNotBB` (pm !! 6) .&. getClosedNeighborhood (lm !! 3)
   in canCooperatePattern1 c pl pm lv lm p || any (canMakeLizhi c pm lm p) (bbToList moveTo)

-- Prometheus specific
hasOneXiangtingPrometheus :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingPrometheus p pl pm lv lm wk = hasOneXiangtingPrometheus' p pl pm lv lm wk || hasOneXiangtingPrometheus'' p pl pm lv lm wk

-- help the other worker
hasOneXiangtingPrometheus' :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingPrometheus' p pl pm lv lm wk =
  let waiter = pl !! p !! (1 - wk)
      mf = pl !! p !! wk
      mfl = lv ! mf
      moveTo = getLegalMoveTo (Just Prometheus) mf pm lm
      waiterNbr = getNeighborhood waiter `andNotBB` (pm !! (5 - p))
      toBeBuiltLv1 = waiterNbr .&. (lm !! 1)
      toBeBuiltLv2 = waiterNbr .&. (lm !! 2)
      toBeBuiltLv3 = waiterNbr .&. (lm !! 3)
   in (lv ! waiter) == 2
        && any
          ( \mt ->
              let firstBuild
                    | (lv ! mt) == mfl = getNeighborhood mf `andNotBB` singletonBB mt
                    | (lv ! mt) < mfl = getNeighborhood mf
                    | otherwise = 0
                  secondBuild = getNeighborhood mt
                  canBuild = firstBuild .|. secondBuild
                  canBuildTwice = firstBuild .&. secondBuild
               in ( (toBeBuiltLv3 /= 0 && ((toBeBuiltLv1 .&. canBuildTwice) .|. (toBeBuiltLv2 .&. canBuild)) /= 0)
                      || (toBeBuiltLv2 .&. firstBuild /= 0 && toBeBuiltLv2 .&. secondBuild /= 0 && countBB (toBeBuiltLv2 .&. canBuild) >= 2)
                  )
          )
          moveTo

-- a worker moves consecutively
hasOneXiangtingPrometheus'' :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingPrometheus'' p pl pm lv lm@[v0, v1, v2, v3, v4, v5, v6, v7] wk =
  let mf = pl !! p !! wk
      mfl = lv ! mf
      moveTo = getLegalMoveTo' mf lm .&. (v1 .|. v2) `andNotBB` (pm !! 6)
      firstBuild = getNeighborhood mf
   in any
        ( \mt ->
            let mtl = lv ! mt
             in if mtl == 1
                  then -- mover raises the move-to level

                    let bb = singletonBB mt
                     in mfl >= 2 && canMakeLizhi Nothing pm [v0, v1 `xor` bb, v2 `xor` bb, v3, v4, v5, v6, v7] p mt
                  else
                    canMakeLizhi Nothing pm lm p mt
                      || ( mfl >= 2
                             && let secondBuild = getNeighborhood mt
                                    canBuild = firstBuild .|. secondBuild
                                    canBuildTwice = firstBuild .&. secondBuild
                                    targetNbr = getNeighborhood mt `andNotBB` (pm !! 6) -- exclude all workers, including mover
                                    a1 = targetNbr .&. (lm !! 1)
                                    a2 = targetNbr .&. (lm !! 2)
                                    a3 = targetNbr .&. (lm !! 3)
                                 in ( mfl == 2
                                        && ( canBuild .&. a3 /= 0 -- no need of double build
                                               || firstBuild .&. a2 /= 0 -- triangle of lv 2
                                           )
                                        || ( mfl == 3
                                               && ( canBuildTwice .&. a1 /= 0 -- double build
                                                      || a2 /= 0 -- lv2 exists
                                                      || a3 /= 0 && targetNbr /= a3
                                                  )
                                           )
                                    )
                         )
        )
        (bbToList moveTo)
hasOneXiangtingPrometheus'' _ _ _ _ _ _ = undefined

-- Minotaur specific
hasOneXiangtingMinotaur :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingMinotaur p pl pm lv lm wk = hasOneXiangtingMinotaur' p pl pm lv lm wk || hasOneXiangtingMinotaur'' p pl pm lv lm wk

-- help the other worker
hasOneXiangtingMinotaur' :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingMinotaur' p pl pm lv lm wk =
  let waiter = pl !! p !! (1 - wk)
      mf = pl !! p !! wk
      mfl = lv ! mf
      moveTo = listToBB $ getLegalMoveTo (Just Minotaur) mf pm lm
      waiterNbrEmpty = getNeighborhood waiter .&. (lm !! 2) `andNotBB` (pm !! 6)
   in (lv ! waiter) == 2 && mfl == 3 && getClosedNeighborhood moveTo .&. waiterNbrEmpty /= 0

-- a worker moves consecutively
hasOneXiangtingMinotaur'' :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingMinotaur'' p pl pm lv lm wk =
  let mf = pl !! p !! wk
      moveTo = getLegalMoveTo (Just Minotaur) mf pm lm
   in any
        ( \mt ->
            (lv ! mt) == 2 -- move-to must be level 2
              && let pl' = case getLegalPushTo (Just Minotaur) pl mf mt of
                       Just (wid, pushTo) -> [[if pp == p && wk == ww then mt else if pp /= p && wid == ww then pushTo else pl !! pp !! ww | ww <- [0, 1]] | pp <- [0, 1]]
                       Nothing -> [[if pp == p && wk == ww then mt else pl !! pp !! ww | ww <- [0, 1]] | pp <- [0, 1]]
                     pm' = createPlayerMap pl'
                     buildAt = listToBB $ getLegalMoveTo (Just Minotaur) mt pm' lm
                     buildAtLv3 = buildAt .&. (lm !! 3)
                     buildAtEmpty = buildAt `andNotBB` (pm' !! 6)
                     buildAtEmptyLv2 = buildAtEmpty .&. (lm !! 2)
                     z3 = countBB buildAtLv3
                     y2 = countBB buildAtEmptyLv2
                     a = countBB buildAtEmpty -- must be >=1, one is move-from
                  in (y2 >= 1 && z3 + y2 >= 2) || (z3 > 2) || (z3 == 2 && a > 2)
        )
        moveTo

-- Artemis specific (maybe slow?)
hasOneXiangtingArtemis :: Int -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
hasOneXiangtingArtemis p pl pm lv lm wk =
  let mf = pl !! p !! wk
      moveTo = getLegalMoveTo (Just Artemis) mf pm lm
   in any
        ( \mt ->
            let -- update player map
                pm' = updatePlayerMap p wk mf mt Nothing pm
                buildAt = getNeighborhood mt .&. (lm !! 7) `andNotBB` (pm' !! 6)
             in any
                  ( \ba ->
                      let -- update level map
                          bal = lv ! ba
                          lm' = makeNextLevelMap lm [(ba, bal, bal + 1)]
                       in hasDoubleLizhi (Just Artemis) p pm' lm'
                  )
                  (bbToList buildAt)
        )
        moveTo

-- Worker x is at level 3 (This happens rarely; opponent is Minotaur, or you are Minotaur and swapped by Apollo)
-- Worker y adjacent to x is at level 2 and trying to win
canCooperatePattern1 :: Maybe Card -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
canCooperatePattern1 (Just Apollo) pl pm lv lm p = canCooperatePattern1' (lm !! 2) (pm !! (4 + p)) pl pm lv lm p
canCooperatePattern1 (Just Hephastus) pl pm lv lm p = canCooperatePattern1' ((lm !! 1) .|. (lm !! 2)) (pm !! 6) pl pm lv lm p
canCooperatePattern1 _ pl pm lv lm p = canCooperatePattern1' (lm !! 2) (pm !! 6) pl pm lv lm p

canCooperatePattern1' :: BitBoard -> BitBoard -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
canCooperatePattern1' targetLevels moveToMask pl pm lv lm p =
  any
    ( \wk ->
        let moverPos = pl !! p !! wk
            waiterPos = pl !! p !! (1 - wk)
            waiterNbr = getNeighborhood waiterPos
            toBeBuilt = waiterNbr .&. targetLevels `andNotBB` (pm !! 6)
            canMoveTo = getLegalMoveTo' moverPos lm `andNotBB` moveToMask
            result = getOpenNeighborhood toBeBuilt .&. canMoveTo /= 0 -- meet-in-the-middle
         in (lv ! moverPos) == 3 && (lv ! waiterPos) == 2 && moverPos `elemBB` waiterNbr && result
    )
    [0, 1]

-- Worker y at level 2 has two or more adjacent empty level-2 spaces.
-- Worker x tries to move and build on top of those spaces.
canCooperatePattern2 :: Card -> Players -> PlayerMap -> Levels -> LevelMap -> Int -> Bool
canCooperatePattern2 Demeter pl pm lv lm p =
  any
    ( \wk ->
        let moverPos = pl !! p !! wk
            waiterPos = pl !! p !! (1 - wk)
            canMoveTo = getLegalMoveTo (Just Demeter) moverPos pm lm
            toBeBuilt = getNeighborhood waiterPos .&. (lm !! 2) `andNotBB` (pm !! (5 - p))
         in (lv ! waiterPos) == 2
              && any
                ( \mt ->
                    countBB (getNeighborhood mt .&. toBeBuilt) >= 2
                )
                canMoveTo
    )
    [0, 1]
canCooperatePattern2 _ _ _ _ _ _ = undefined

canMakeLizhi :: Maybe Card -> PlayerMap -> LevelMap -> Int -> Index -> Bool
canMakeLizhi c pm lm p mt =
  let ours = pm !! (4 + p)
      opponent = pm !! (5 - p)
      buildAt = getNeighborhood mt .&. (lm !! 7) `andNotBB` opponent
      ba0 = buildAt .&. (lm !! 0)
      ba1 = buildAt .&. (lm !! 1)
      ba2 = buildAt .&. (lm !! 2)
      ba3 = buildAt .&. (lm !! 3)
      bx0 = ba0 .&. ours
      bx1 = ba1 .&. ours
      bx2 = ba2 .&. ours
      bx3 = ba3 .&. ours
      x0 = countBB bx0
      x1 = countBB bx1
      x2 = countBB bx2
      x3 = countBB bx3
      y0 = countBB $ ba0 `xor` bx0
      y1 = countBB $ ba1 `xor` bx1
      y2 = countBB $ ba2 `xor` bx2
      y3 = countBB $ ba3 `xor` bx3
   in canMakeLizhi' c (mt `elemBB` opponent) x0 x1 x2 x3 y0 y1 y2 y3

canMakeLizhi' :: Maybe Card -> Bool -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
canMakeLizhi' (Just Apollo) True _ x1 x2 x3 y0 y1 y2 y3 =
  (x1 >= 1 && y3 >= 1 && y2 + y3 >= 2) -- coming from lv 1
    || (x2 >= 1 && y3 >= 1 && y2 + y3 >= 2) -- coming from lv 2
    || (x3 >= 1 && (y2 >= 1 || (y3 >= 1 && y0 + y1 + y3 >= 2))) -- coming form lv3
canMakeLizhi' (Just Demeter) _ _ x1 x2 x3 y0 y1 y2 y3 =
  (x1 >= 1 && y2 + y3 >= 2) -- coming from lv 1
    || (x2 >= 1 && y2 + y3 >= 1) -- coming from lv 2
    || (x3 >= 1 && (y2 >= 1 || (y3 >= 1 && y0 + y1 + y3 >= 2))) -- coming form lv3
canMakeLizhi' (Just Hephastus) _ _ x1 x2 x3 y0 y1 y2 y3 =
  (x1 + x2 >= 1 && y3 >= 1) -- coming from lv 1 or 2
    || (x3 >= 1 && (y1 + y2 >= 1 || (y3 >= 1 && y0 + y3 >= 2))) -- coming form lv3
canMakeLizhi' (Just Pan) _ _ x1 x2 x3 y0 y1 y2 y3 =
  (x1 >= 1 && y0 + y3 >= 1 && y0 + y2 + y3 >= 2) -- coming from lv 1
    || (x2 >= 1 && y0 + y3 >= 1) -- coming from lv 2
    || (x3 >= 1 && (y2 >= 1 || (y0 + y3 >= 1 && y0 + y1 + y3 >= 2))) -- coming form lv3
canMakeLizhi' _ _ _ x1 x2 x3 y0 y1 y2 y3 =
  (x1 >= 1 && y3 >= 1 && y2 + y3 >= 2) -- coming from lv 1
    || (x2 >= 1 && y3 >= 1) -- coming from lv 2
    || (x3 >= 1 && (y2 >= 1 || (y3 >= 1 && y0 + y1 + y3 >= 2))) -- coming form lv3

-- (7) Prometeus Bonus
evaluatePrometeusBonus :: Cards -> PlayerMap -> LevelMap -> Int -> Score
evaluatePrometeusBonus cs pm lm p =
  if cs !! p == Just Prometheus
    then
      let lv3nbr = getClosedNeighborhood (lm !! 3) `andNotBB` (pm !! (5 - p))
       in if getClosedNeighborhood (lv3nbr .&. (pm !! (4 + p)) .&. (lm !! 1)) .&. (lm !! 2) `andNotBB` (pm !! 6) /= 0
            then -- Pattern 3-*1-2
              evalPrometeus312
            else
              if getClosedNeighborhood (lv3nbr .&. ((lm !! 1) .|. (lm !! 2))) .&. (lm !! 2) .&. (pm !! (4 + p)) /= 0
                then evalPrometeus322 -- Pattern 3-(1or2)-*2
                else 0
    else 0

-- (8) Stepping Bonus
evaluateStepping :: Cards -> Players -> PlayerMap -> Levels -> LevelMap -> [IntMap Int] -> Int -> Score
evaluateStepping cs ps pm lv lm dist p = sum [f i | w <- [0, 1], let i = ps !! p !! w]
  where
    f i = evaluateStepping' (cs !! p) (lv ! i) (getNeighborhood i `andNotBB` (pm !! 6)) lm (dist !! (1 - p))

evaluateStepping' :: Maybe Card -> Int -> BitBoard -> LevelMap -> IntMap Int -> Score
evaluateStepping' c 2 mask lm dist
  | lv2nbr /= 0 && oppUnreachable lv2nbr = evalStepping22
  | c == Just Prometheus && lv1nbr /= 0 && oppUnreachable lv1nbr = evalStepping22
  | otherwise = 0
  where
    lv2nbr = mask .&. (lm !! 2)
    lv1nbr = mask .&. (lm !! 1)
    oppUnreachable bb = any (\i -> dist ! i > 1) (bbToList bb)
evaluateStepping' c 1 mask lm dist
  | lv2nbr /= 0 && oppUnreachable lv2nbr = evalStepping12
  | lv1nbr /= 0 && oppUnreachable lv1nbr = evalStepping11
  | c == Just Prometheus && lv0nbr /= 0 && oppUnreachable lv0nbr = evalStepping11
  where
    lv2nbr = mask .&. (lm !! 2)
    lv1nbr = mask .&. (lm !! 1)
    lv0nbr = mask .&. (lm !! 0)
    oppUnreachable bb = any (\i -> dist ! i > 1) (bbToList bb)
evaluateStepping' _ 0 mask lm dist
  | lv1nbr /= 0 && oppUnreachable lv1nbr = evalStepping01
  | otherwise = 0
  where
    lv1nbr = mask .&. (lm !! 1)
    oppUnreachable bb = any (\i -> dist ! i > 1) (bbToList bb)
evaluateStepping' _ _ _ _ _ = 0

--------------------------------------------------------------------------------
-- For unit testing
--------------------------------------------------------------------------------

evaluateWorkerProximity' :: GameState -> Int -> Score
evaluateWorkerProximity' GameState {cards = cs, players = pl, playerMap = pm, levelMap = lm} = evaluateWorkerProximity pl (fst $ getDistances cs pl pm (createAdjacencyList lm))

evaluateReachability' :: GameState -> Int -> Score
evaluateReachability' GameState {cards = cs, players = pl, playerMap = pm, levels = lv, levelMap = lm} = evaluateReachability cs lv (snd $ getDistances cs pl pm (createAdjacencyList lm))

evaluateAsymmetry' :: GameState -> Int -> Score
evaluateAsymmetry' GameState {cards = cs, players = pl, playerMap = pm, levels = lv, levelMap = lm} = evaluateAsymmetry pl lv lm (snd $ getDistances cs pl pm (createAdjacencyList lm))

evaluateStuckBonus' :: GameState -> Int -> Score
evaluateStuckBonus' GameState {players = pl, levelMap = lm} = evaluateStuckBonus pl (createAdjacencyList lm)

evaluatePrevention' :: GameState -> Int -> Score
evaluatePrevention' GameState {cards = cs, players = pl, playerMap = pm, levels = lv, levelMap = lm} = evaluatePrevention pl lv lm (snd $ getDistances cs pl pm (createAdjacencyList lm))
