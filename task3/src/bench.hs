{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Criterion.Main
import Data.Bits (Bits ((.&.), (.|.)), complement)
import qualified Data.Board as B
import Data.Card
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.List (foldl', groupBy, sort)
import Game.BitBoard (BitBoard, andNotBB, bbToList, elemBB, getClosedNeighborhood, getNeighborhood, listToBB, singletonBB)
import Game.GameMove
import Game.GameState (GameState (GameState, cards, legalMoves, levelMap, playerMap, players), fromBoard, makeMove)
import Search.Search
import Game.Evaluation (getDistances', getDistancesBB)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    chooseInt,
    elements,
    generate,
    shuffle,
    vectorOf,
  )

type Index = (Int, Int)

type Level = Int

toIndex :: Int -> Index
toIndex i = (i `div` 5 + 1, i `mod` 5 + 1)

fromIndex :: Index -> Int
fromIndex (x, y) = (x - 1) * 5 + (y -1)

-- Naive
getArtemisMoveToNaive :: (Index, [Index], [Level]) -> [Index]
getArtemisMoveToNaive (moveFrom, workers, spaces) =
  let nr1 = nbr moveFrom
   in (nubOrd . concat) $ nr1 : [nbr p | p <- nr1, (levels ! fromIndex p) /= 3 || (levels ! fromIndex moveFrom) /= 2]
  where
    levels = Map.fromList $ zip [0 ..] spaces
    nbr (x, y) =
      [ (xx, yy)
        | dx <- [-1, 0, 1],
          dy <- [-1, 0, 1],
          dx /= 0 || dy /= 0,
          let xx = x + dx, -- move-to candidate
          let yy = y + dy,
          1 <= xx && xx <= 5, -- boundary check
          1 <= yy && yy <= 5,
          levels ! fromIndex (xx, yy) <= 3, -- cannot move up to level 4
          levels ! fromIndex (x, y) + 1 >= levels ! fromIndex (xx, yy), -- can move up at most one level
          (xx, yy) `notElem` workers -- space must be unoccupied
      ]

-- Bitboard
toBBIndex :: Index -> Int
toBBIndex (x, y) = x * 7 + y

fromBBIndex :: Int -> Index
fromBBIndex i = (i `div` 7, i `mod` 7)

getArtemisMoveToBB :: (Index, [Index], [Level]) -> [Index]
getArtemisMoveToBB = convertOutput . getArtemisMoveToBB' . convertInput

getArtemisMoveToBB2 :: (Index, [Index], [Level]) -> [Index]
getArtemisMoveToBB2 = convertOutput . getArtemisMoveToBB2' . convertInput2

getArtemisMoveToBB' :: (Int, BitBoard, IntMap Level, IntMap BitBoard) -> BitBoard
getArtemisMoveToBB' (mfrom, unoccupiedBB, lv, lm) =
  let firstMove = nbr mfrom
      secondMoveFrom = (if lv ! mfrom == 2 then (\x -> x `andNotBB` (lm ! 3)) else id) firstMove
   in foldl' (\z x -> z .|. nbr x) firstMove (bbToList secondMoveFrom)
  where
    nbr mf = getNeighborhood mf .&. (lm ! min 7 ((lv ! mf) + 5)) .&. unoccupiedBB

getArtemisMoveToBB2' :: (Int, BitBoard, [BitBoard]) -> BitBoard
getArtemisMoveToBB2' (mfrom, unoccupiedBB, lm) =
  let firstMove = nbr mfrom
      secondMoveFrom = (if mfrom `elemBB` (lm !! 2) then (\x -> x `andNotBB` (lm !! 3)) else id) firstMove
   in foldl' (\z x -> z .|. nbr x) firstMove (bbToList secondMoveFrom)
  where
    canMoveTo i
      | i `elemBB` (lm !! 0) = lm !! 5
      | i `elemBB` (lm !! 1) = lm !! 6
      | otherwise = lm !! 7
    nbr mf = getNeighborhood mf .&. canMoveTo mf .&. unoccupiedBB

getArtemisMoveToFinal :: (Index, [Index], [Level]) -> [Index]
getArtemisMoveToFinal = convertOutput . getArtemisMoveToFinal' . convertInput

getArtemisMoveToFinal2 :: (Index, [Index], [Level]) -> [Index]
getArtemisMoveToFinal2 = convertOutput . getArtemisMoveToFinal2' . convertInput2

convertInput :: (Index, [Index], [Level]) -> (Int, BitBoard, IntMap Level, IntMap BitBoard)
convertInput (moveFrom, workers, spaces) =
  (toBBIndex moveFrom, workersBB', lv, lm)
  where
    workersBB = listToBB $ map toBBIndex workers
    workersBB' = complement workersBB
    lv = Map.fromList [(toBBIndex (toIndex i), z) | (i, z) <- zip [0 ..] spaces]
    m' = Map.fromListWith (.|.) [(v, singletonBB k) | (k, v) <- Map.toList lv]
    m = Map.union m' (Map.fromList [(l, 0) | l <- [0 .. 4]])
    v5 = (m ! 0) .|. (m ! 1) -- level 0 or 1
    v6 = v5 .|. (m ! 2) -- level 0, 1 or 2
    v7 = v6 .|. (m ! 3) -- level 0, 1, 2 or 3
    lm = Map.insert 5 v5 $ Map.insert 6 v6 $ Map.insert 7 v7 m

adjustListElement :: Int -> (a -> a) -> [a] -> [a]
adjustListElement i f xs = case splitAt i xs of
  (a, x : b) -> a ++ (f x : b)
  _ -> xs

convertInput2 :: (Index, [Index], [Level]) -> (Int, BitBoard, [BitBoard])
convertInput2 (moveFrom, workers, spaces) = (toBBIndex moveFrom, unoccupied, lm)
  where
    unoccupied = complement $ listToBB $ map toBBIndex workers
    (v0, v1, v2, v3, v4) =
      foldl'
        ( \(x0, x1, x2, x3, x4) (i, lv) ->
            let bb = singletonBB ((i `div` 5 + 1) * 7 + (i `mod` 5 + 1))
             in case lv of
                  0 -> (x0 + bb, x1, x2, x3, x4)
                  1 -> (x0, x1 + bb, x2, x3, x4)
                  2 -> (x0, x1, x2 + bb, x3, x4)
                  3 -> (x0, x1, x2, x3 + bb, x4)
                  _ -> (x0, x1, x2, x3, x4 + bb)
        )
        (0, 0, 0, 0, 0)
        (zip [0 ..] spaces)
    v5 = v0 + v1 -- level 0 or 1
    v6 = v5 + v2 -- level 0, 1 or 2
    v7 = v6 + v3 -- level 0, 1, 2 or 3
    lm = [v0, v1, v2, v3, v4, v5, v6, v7]

convertOutput :: BitBoard -> [Index]
convertOutput = map fromBBIndex . bbToList

getArtemisMoveToFinal' :: (Int, BitBoard, IntMap Level, IntMap BitBoard) -> BitBoard
getArtemisMoveToFinal' (mf, unoccupiedBB, _, lm) =
  let (xs, a) = nbr (mfBB .&. (lm ! 0), mfBB .&. (lm ! 1), mfBB .&. (lm ! 2), mfBB .&. (lm ! 3))
      ((y0, y1, y2, y3), b) = nbr xs
   in y0 .|. y1 .|. y2 .|. y3 .|. a .|. b
  where
    mfBB = singletonBB mf
    nbr (x0, x1, x2, x3) =
      let y0 = getClosedNeighborhood (x0 .|. x1 .|. x2 .|. x3) .&. (lm ! 0) .&. unoccupiedBB
          y1 = getClosedNeighborhood (x0 .|. x1 .|. x2 .|. x3) .&. (lm ! 1) .&. unoccupiedBB
          y2 = getClosedNeighborhood (x1 .|. x2 .|. x3) .&. (lm ! 2) .&. unoccupiedBB
          y3 = getClosedNeighborhood x3 .&. (lm ! 3) .&. unoccupiedBB
          z = getClosedNeighborhood x2 .&. (lm ! 3) .&. unoccupiedBB
       in ((y0, y1, y2, y3), z)

getArtemisMoveToFinal2' :: (Int, BitBoard, [BitBoard]) -> BitBoard
getArtemisMoveToFinal2' (mf, unoccupiedBB, lm) =
  let (xs, a) = nbr (mfBB .&. (lm !! 0), mfBB .&. (lm !! 1), mfBB .&. (lm !! 2), mfBB .&. (lm !! 3))
      ((y0, y1, y2, y3), b) = nbr xs
   in (y0 + y1 + y2 + y3 + a) .|. b
  where
    mfBB = singletonBB mf
    nbr (x0, x1, x2, x3) =
      let y0 = getClosedNeighborhood (x0 + x1 + x2 + x3) .&. (lm !! 0) .&. unoccupiedBB
          y1 = getClosedNeighborhood (x0 + x1 + x2 + x3) .&. (lm !! 1) .&. unoccupiedBB
          y2 = getClosedNeighborhood (x1 + x2 + x3) .&. (lm !! 2) .&. unoccupiedBB
          y3 = getClosedNeighborhood x3 .&. (lm !! 3) .&. unoccupiedBB
          z = getClosedNeighborhood x2 .&. (lm !! 3) .&. unoccupiedBB
       in ((y0, y1, y2, y3), z)

-- Generator
generateRandomInput :: Int -> IO [(Index, [Index], [Level])]
generateRandomInput n = generate $
  vectorOf n $ do
    indices <- shuffle [0 .. 24]
    tokenLevels <- vectorOf 4 $ chooseInt (0, 2) -- levels at token positions
    emptyLevels <- vectorOf 21 $ elements [0, 0, 1, 1, 1, 2, 2, 2, 3, 4] -- levels at empty spaces (choose with some biases)
    let spaces = (map snd . sort . zip indices) (tokenLevels ++ emptyLevels)
    let tokens = [toIndex i | i <- take 4 indices]
    return (head tokens, tokens, spaces)

main :: IO ()
-- =============================================================================
--     1
-- =============================================================================
-- main = do
--   instances <- generateRandomInput 10000
--   let convertedInstances = map convertInput instances
--   let convertedInstances2 = map convertInput2 instances

--   -- check correctness
--   let expected = map (sort . getArtemisMoveToNaive) instances
--   if all
--     ( \(x, e) ->
--         sort (getArtemisMoveToBB x) == e
--           && sort (getArtemisMoveToFinal x) == e
--           && sort (getArtemisMoveToBB2 x) == e
--           && sort (getArtemisMoveToFinal2 x) == e
--     )
--     (zip instances expected)
--     then do
--       -- measurement
--       putStrLn "correct"
--       defaultMain
--         [ bgroup
--             "getArtemisMoveTo"
--             [ bench "Naive" $ nf (map getArtemisMoveToNaive) instances,
--               -- bench "BB" $ nf (map getArtemisMoveToBB) instances,
--               -- bench "BB (converted)" $ nf (map getArtemisMoveToBB') convertedInstances,
--               bench "BB 2" $ nf (map getArtemisMoveToBB2) instances,
--               bench "BB 2 (precomputed)" $ nf (map getArtemisMoveToBB2') convertedInstances2,
--               -- bench "Final" $ nf (map getArtemisMoveToFinal) instances,
--               -- bench "Final (converted)" $ nf (map getArtemisMoveToFinal') convertedInstances,
--               bench "Final 2" $ nf (map getArtemisMoveToFinal2) instances,
--               bench "Final 2 (precomputed)" $ nf (map getArtemisMoveToFinal2') convertedInstances2
--             ]
--         ]
--     else do
--       putStrLn "failed"

-- =============================================================================
--     2
-- =============================================================================

-- main = do
--   -- game opening
--   let b1 =
--         B.Board
--           { B.players =
--               ( B.Player {B.card = Artemis, B.tokens = Just ((4, 4), (5, 5))},
--                 B.Player {B.card = Prometheus, B.tokens = Just ((3, 4), (4, 3))}
--               ),
--             B.spaces = [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]],
--             B.turn = 0
--           }
--   let s1 = fromBoard b1

--   -- close to end-game
--   let b2 =
--         B.Board
--           { B.players =
--               ( B.Player {B.card = Artemis, B.tokens = Just ((3, 5), (5, 2))},
--                 B.Player {B.card = Prometheus, B.tokens = Just ((3, 2), (4, 4))}
--               ),
--             B.spaces = [[0, 3, 0, 0, 0], [3, 0, 0, 0, 0], [0, 1, 0, 4, 1], [0, 4, 2, 2, 0], [1, 0, 4, 0, 0]],
--             B.turn = 20
--           }
--   let s2 = fromBoard b1

--   defaultMain
--     [ bgroup
--         "search opening"
--         [ bench "ɑ-β: depth 1" $ nf (searchAlphaBeta s1) 1,
--           bench "ɑ-β: depth 2" $ nf (searchAlphaBeta s1) 2,
--           bench "ɑ-β: depth 3" $ nf (searchAlphaBeta s1) 3
--         ],
--       bgroup
--         "search end-game"
--         [ bench "ɑ-β: depth 1" $ nf (searchAlphaBeta s2) 1,
--           bench "ɑ-β: depth 2" $ nf (searchAlphaBeta s2) 2,
--           bench "ɑ-β: depth 3" $ nf (searchAlphaBeta s2) 3
--         ]
--     ]

-- =============================================================================
--     3
-- =============================================================================
main = do
  boards <- generate $ vectorOf 10000 (arbitrary :: Gen B.Board)
  instances <- evaluate $! map fromBoard boards

  let f1 GameState {cards = cs, players = pl, playerMap = pm, levelMap = lm} = getDistances' cs pl pm lm
  let f2 GameState {cards = cs, players = pl, playerMap = pm, levelMap = lm} = getDistancesBB cs pl pm lm

  defaultMain
    [ bgroup
        "getDistances()"
        [ bench "getDistances'" $ nf (map f1) instances,
          bench "getDistancesBB" $ nf (map f2) instances
        ]
    ]
