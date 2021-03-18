module Game.EvaluationSpec (spec) where

import Control.Monad (forM_)
-- import Data.Bits (Bits (xor, (.&.)))
import Data.Board (readBoard)
import qualified Data.Board as B
import Data.Card
-- import Data.IntMap ((!))
import qualified Data.IntMap as Map
-- import Data.List (foldl', maximumBy)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Game.BitBoard
import Game.Evaluation
import Game.GameMove
import Game.GameState
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- rc to index
ri :: Int -> Int -> Int
ri r c = posToIndex (r, c)

eval :: String -> Int
eval s = evaluate $ fromBoard (fromMaybe undefined (readBoard s))

-- printInfo :: String -> IO ()
-- printInfo s =
--   let b = fromBoard (fromMaybe undefined (readBoard s))
--       fs = [evaluateAsymmetry', evaluatePrevention', evaluateReachability', evaluateStuckBonus', evaluateWorkerProximity']
--    in print [[f b p | f <- fs] | p <- [0, 1]]

spec :: Spec
spec = do
  describe "Evaluation#bfsBB()" $ do
    it "computes distances" $ do
      let pm1 = createPlayerMap [[ri 1 1, ri 3 3], [ri 5 1, ri 5 5]]
      let lv1 =
            Map.fromList $
              (zip validIndices . concat)
                [ [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      let lm1 = createLevelMap lv1
      bfsBB [Nothing, Nothing] 0 (Just 1) pm1 lm1
        `shouldBe` [ listToBB [ri 3 3],
                     listToBB [ri 2 2, ri 2 3, ri 2 4, ri 3 2, ri 3 4, ri 4 2, ri 4 3, ri 4 4],
                     listToBB [ri 1 1, ri 1 2, ri 1 3, ri 1 4, ri 1 5, ri 2 1, ri 2 5, ri 3 1, ri 3 5, ri 4 1, ri 4 5, ri 5 2, ri 5 3, ri 5 4]
                   ]

      bfsBB [Just Apollo, Nothing] 0 (Just 1) pm1 lm1
        `shouldBe` [ listToBB [ri 3 3],
                     listToBB [ri 2 2, ri 2 3, ri 2 4, ri 3 2, ri 3 4, ri 4 2, ri 4 3, ri 4 4],
                     listToBB [ri 1 1, ri 1 2, ri 1 3, ri 1 4, ri 1 5, ri 2 1, ri 2 5, ri 3 1, ri 3 5, ri 4 1, ri 4 5, ri 5 1, ri 5 2, ri 5 3, ri 5 4, ri 5 5]
                   ]

      let pm2 = createPlayerMap [[ri 1 1, ri 1 5], [ri 2 2, ri 5 4]]
      let lv2 =
            Map.fromList $
              (zip validIndices . concat)
                [ [0, 2, 3, 3, 0],
                  [0, 0, 1, 4, 1],
                  [1, 4, 0, 4, 0],
                  [0, 4, 1, 4, 1],
                  [1, 2, 0, 0, 2]
                ]
      let lm2 = createLevelMap lv2
      bfsBB [Nothing, Nothing] 0 (Just 0) pm2 lm2
        `shouldBe` [ listToBB [ri 1 1],
                     listToBB [ri 2 1],
                     listToBB [ri 3 1],
                     listToBB [ri 4 1],
                     listToBB [ri 5 1],
                     listToBB [ri 5 2],
                     listToBB [ri 4 3, ri 5 3],
                     listToBB [ri 3 3],
                     listToBB [ri 2 3],
                     listToBB [ri 1 2],
                     listToBB [ri 1 3],
                     listToBB [ri 1 4],
                     listToBB [ri 1 5, ri 2 5],
                     listToBB [ri 3 5],
                     listToBB [ri 4 5],
                     listToBB [ri 5 5]
                   ]
      bfsBB [Just Minotaur, Nothing] 0 (Just 0) pm2 lm2
        `shouldBe` [ listToBB [ri 1 1],
                     listToBB [ri 2 1, ri 2 2],
                     listToBB [ri 3 1, ri 2 3, ri 3 3],
                     listToBB [ri 4 1, ri 1 2, ri 4 3],
                     listToBB [ri 5 1, ri 5 2, ri 1 3, ri 5 3, ri 5 4],
                     listToBB [ri 1 4, ri 4 5],
                     listToBB [ri 1 5, ri 2 5, ri 3 5, ri 5 5]
                   ]
      bfsBB [Just Artemis, Nothing] 0 (Just 0) pm2 lm2
        `shouldBe` [ listToBB [ri 1 1],
                     listToBB [ri 2 1, ri 3 1],
                     listToBB [ri 4 1, ri 5 1],
                     listToBB [ri 5 2, ri 4 3, ri 5 3],
                     listToBB [ri 3 3, ri 2 3],
                     listToBB [ri 1 2, ri 1 3],
                     listToBB [ri 1 4, ri 1 5, ri 2 5],
                     listToBB [ri 3 5, ri 4 5],
                     listToBB [ri 5 5]
                   ]

  describe "Evaluation#compactDistanceForArtemis()" $ do
    it "can be taken the first few elements" $ do
      let xs = cycle [singletonBB (ri 1 1), singletonBB (ri 1 2)]
      let xx = singletonBB (ri 1 1) + singletonBB (ri 1 2)
      take 4 (compactDistanceForArtemis xs) `shouldBe` [singletonBB (ri 1 1), xx, xx, xx]

  describe "Evaluation#getDistancesBB()" $ do
    prop "returns the same result as getDistances()" $ \b ->
      let GameState {cards = cs, players = pl, playerMap = pm, levelMap = lm} = fromBoard b
       in getDistancesBB cs pm lm `shouldBe` getDistances' cs pl pm lm

  describe "Evaluation#evaluate()" $ do
    it "finds out end game" $ do
      -- no available moves
      let s11 = "{\"turn\":8,\"spaces\":[[0,0,2,0,0],[0,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[1,4],[1,5]]},{\"card\":\"Prometheus\",\"tokens\":[[3,3],[3,4]]}]}"
      let s12 = "{\"turn\":9,\"spaces\":[[0,0,2,0,0],[1,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[1,4],[1,5]]},{\"card\":\"Prometheus\",\"tokens\":[[3,3],[3,4]]}]}"

      eval s11 `shouldBe` (- scoreWin)
      eval s12 `shouldBe` scoreWin

    it "prefers building farther from opponent" $ do
      let s1 = "{\"turn\":1,\"spaces\":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,5],[5,5]]},{\"card\":\"Prometheus\",\"tokens\":[[1,2],[2,2]]}]}"
      let s2 = "{\"turn\":1,\"spaces\":[[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,5],[5,5]]},{\"card\":\"Prometheus\",\"tokens\":[[1,2],[2,2]]}]}"
      let s3 = "{\"turn\":1,\"spaces\":[[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,5],[5,5]]},{\"card\":\"Prometheus\",\"tokens\":[[2,1],[2,2]]}]}"

      eval s2 < eval s1 `shouldBe` True
      eval s3 < eval s2 `shouldBe` True

    it "prefers building next to the boundary" $ do
      let s21 = "{\"turn\":3,\"spaces\":[[1,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,4],[4,5]]},{\"card\":\"Prometheus\",\"tokens\":[[1,1],[2,2]]}]}"
      let s22 = "{\"turn\":3,\"spaces\":[[1,0,0,0,0],[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,4],[4,5]]},{\"card\":\"Prometheus\",\"tokens\":[[2,2],[3,3]]}]}"

      let b21 = fromBoard (fromMaybe undefined (readBoard s21))
      let b22 = fromBoard (fromMaybe undefined (readBoard s22))

      evaluateWorkerProximity' b21 0 `shouldBe` evaluateWorkerProximity' b21 1
      evaluateWorkerProximity' b21 0 `shouldBe` evaluateWorkerProximity' b22 0
      evaluateWorkerProximity' b22 0 `shouldBe` evaluateWorkerProximity' b22 1

      evaluateReachability' b22 0 < evaluateReachability' b21 0 `shouldBe` True
      evaluateReachability' b21 0 < evaluateReachability' b21 1 `shouldBe` True
      evaluateReachability' b21 1 < evaluateReachability' b22 1 `shouldBe` True

      evaluateAsymmetry' b22 0 < evaluateAsymmetry' b21 0 `shouldBe` True
      evaluateAsymmetry' b21 0 < evaluateAsymmetry' b22 1 `shouldBe` True
      evaluateAsymmetry' b22 1 < evaluateAsymmetry' b21 1 `shouldBe` True

      -- s21 is better for Player1
      evaluate b21 > evaluate b22 `shouldBe` True

    it "prefers preventing opponent from climbing up" $ do
      let s31 = "{\"turn\":2,\"spaces\":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,1,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[2,1],[2,2]]},{\"card\":\"Prometheus\",\"tokens\":[[2,3],[3,3]]}]}"
      let s32 = "{\"turn\":2,\"spaces\":[[2,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[2,1],[2,2]]},{\"card\":\"Prometheus\",\"tokens\":[[1,2],[3,2]]}]}"

      eval s32 < eval s31 `shouldBe` True

    it "prefers climbing up" $ do
      let s41 = "{\"players\":[{\"card\":\"Atlas\",\"tokens\":[[3,2],[4,1]]},{\"card\":\"Prometheus\",\"tokens\":[[4,4],[4,5]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,2]],\"turn\":3}"
      let s42 = "{\"players\":[{\"card\":\"Atlas\",\"tokens\":[[3,2],[4,1]]},{\"card\":\"Prometheus\",\"tokens\":[[4,4],[5,5]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,1],[0,0,0,0,1]],\"turn\":3}"
      -- printInfo s41
      -- printInfo s42

      -- s42 is better for Player 1
      eval s41 < eval s42 `shouldBe` True

      let s43 = "{\"players\":[{\"card\":\"Atlas\",\"tokens\":[[2,2],[4,2]]},{\"card\":\"Prometheus\",\"tokens\":[[4,4],[4,5]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,2]],\"turn\":3}"
      let s44 = "{\"players\":[{\"card\":\"Atlas\",\"tokens\":[[2,2],[4,2]]},{\"card\":\"Prometheus\",\"tokens\":[[4,4],[5,5]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1],[0,0,1,0,1]],\"turn\":3}"

      -- eval s44 `shouldBe` 2005

      -- printInfo s43
      -- printInfo s44
      eval s44 > eval s43 `shouldBe` True

    it "works with several initial positions" $ do
      -- let s51 = "{\"turn\":0,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,4],[5,5]]},{\"card\":\"Prometheus\",\"tokens\":[[1,1],[2,2]]}]}"
      -- let b51 = fromBoard (fromMaybe undefined (readBoard s51))

      -- let nextStates = [(m, makeMove b51 m) | m <- legalMoves b51]
      -- let nextMove = minimumBy (comparing fst) [(evaluate st, m) | (m, st) <- nextStates]

      -- print $ sortBy (comparing fst) [(evaluate st, m) | (m, st) <- nextStates]

      let s52 = "{\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,4],[5,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,2],[4,2]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,1]],\"turn\":2}"
      let b52 = fromBoard (fromMaybe undefined (readBoard s52))
      -- print [((-1) * evaluate (makeMove b52 m), m) | m <- legalMoves b52]
      -- print $ maximumBy (comparing fst) [((-1) * evaluate (makeMove b52 m), m) | m <- legalMoves b52]
      let bestMove = snd $ maximumBy (comparing fst) [((-1) * evaluate (makeMove b52 m), m) | m <- legalMoves b52]
      bestMove `elem` legalMoves b52 `shouldBe` True

    -- print (evaluate (makeMove b52 bestMove))
    -- print $ writeBoard (toBoard (makeMove b52 bestMove))

    it "makeMove should not affect the evaluation" $ do
      let s52 = "{\"players\":[{\"card\":\"Atlas\",\"tokens\":[[4,4],[5,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,2],[4,2]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,1]],\"turn\":2}"
      let b52 = fromBoard (fromMaybe undefined (readBoard s52))

      -- worker 2, (5,4) -> (4,5) build (5,5)
      let b52' = makeMove b52 $ (setWorkerId 1 . setMoveFrom 39 . setMoveTo 33 . setBuildAt [(40, 1, 2)]) createGameMove

      let s53 = "{\"players\":[{\"card\":\"Prometheus\",\"tokens\":[[2,2],[4,2]]},{\"card\":\"Atlas\",\"tokens\":[[4,4],[4,5]]}],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,2]],\"turn\":3}"
      let b53 = fromBoard (fromMaybe undefined (readBoard s53))

      toBoard b52' `shouldBe` toBoard b53
      b52' `shouldBe` b53
      eval s53 `shouldBe` evaluate b52'

    -- it "prefers early builders" $ do
    --   let b1 =
    --         B.Board
    --           { B.players =
    --               ( B.Player {B.card = Prometheus, B.tokens = Just ((5, 4), (5, 1))},
    --                 B.Player {B.card = Apollo, B.tokens = Just ((1, 3), (3, 2))}
    --               ),
    --             B.spaces = [[4, 2, 1, 4, 0], [4, 1, 4, 4, 0], [4, 1, 4, 1, 0], [4, 2, 2, 4, 0], [2, 4, 4, 1, 2]],
    --             B.turn = 45
    --           }
    --   let s1 = fromBoard b1

    --   let b2 =
    --         B.Board
    --           { B.players =
    --               ( B.Player {B.card = Prometheus, B.tokens = Just ((5, 4), (5, 1))},
    --                 B.Player {B.card = Apollo, B.tokens = Just ((1, 2), (4, 2))}
    --               ),
    --             B.spaces = [[4, 2, 1, 4, 0], [4, 0, 4, 4, 0], [4, 1, 4, 1, 0], [4, 2, 3, 4, 0], [2, 4, 4, 1, 2]],
    --             B.turn = 45
    --           }
    --   let s2 = fromBoard b2

    --   -- print $ evaluateDetails s1
    --   -- print $ evaluate s1
    --   -- print $ evaluateDetails s2
    --   -- print $ evaluate s2

    --   evaluate s1 > evaluate s2 `shouldBe` True
    it "prevent opponent Hephastus from winning" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Hephastus, B.tokens = Just ((4, 4), (4, 2))},
                    B.Player {B.card = Atlas, B.tokens = Just ((2, 1), (2, 2))}
                  ),
                B.spaces = [[2, 0, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 2, 0, 0], [0, 0, 4, 1, 0], [0, 0, 0, 0, 2]],
                B.turn = 9
              }
      let s1 = fromBoard b1

      let b2 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Hephastus, B.tokens = Just ((4, 4), (4, 2))},
                    B.Player {B.card = Atlas, B.tokens = Just ((2, 4), (3, 2))}
                  ),
                B.spaces = [[1, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 4, 4, 0], [0, 0, 4, 1, 0], [0, 0, 0, 0, 0]],
                B.turn = 9
              }
      let s2 = fromBoard b2

      -- print $ evaluateDetails s1
      -- print $ evaluate s1
      -- print $ evaluateDetails s2
      -- print $ evaluate s2

      evaluate s1 < evaluate s2 `shouldBe` True

  describe "Evaluation#hasDoubleLizhi()" $ do
    it "Works with Pan" $ do
      let pm1 = createPlayerMap [[ri 1 1, ri 1 3], [ri 5 1, ri 5 2]]
      let lv1 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 2, 2, 2],
                  [4, 4, 1, 1, 1],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      let lm1 = createLevelMap lv1
      hasDoubleLizhi (Just Pan) 0 pm1 lm1 `shouldBe` False

      let pm2 = createPlayerMap [[ri 1 1, ri 1 4], [ri 5 1, ri 5 2]]
      let lv2 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 4, 1, 2, 3],
                  [1, 0, 1, 1, 1],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      let lm2 = createLevelMap lv2
      hasDoubleLizhi (Just Pan) 0 pm2 lm2 `shouldBe` True

      let pm3 = createPlayerMap [[ri 1 1, ri 1 4], [ri 5 1, ri 5 2]]
      let lv3 =
            Map.fromList $
              (zip validIndices . concat)
                [ [3, 1, 1, 1, 2],
                  [0, 3, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      let lm3 = createLevelMap lv3
      hasDoubleLizhi (Just Pan) 0 pm3 lm3 `shouldBe` True

    it "Works with Artemis" $ do
      -- no instances
      let pm1 = createPlayerMap [[ri 1 1, ri 1 5], [ri 5 1, ri 5 2]]
      let lv1 =
            Map.fromList $
              (zip validIndices . concat)
                [ [1, 2, 3, 4, 3],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm1 (createLevelMap lv1) `shouldBe` False

      let pm2 = createPlayerMap [[ri 1 1, ri 1 3], [ri 5 1, ri 5 2]]
      let lv2 =
            Map.fromList $
              (zip validIndices . concat)
                [ [1, 2, 3, 2, 3],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm2 (createLevelMap lv2) `shouldBe` False

      let pm2' = createPlayerMap [[ri 1 1, ri 1 5], [ri 1 2, ri 2 4]]
      let lv2' =
            Map.fromList $
              (zip validIndices . concat)
                [ [3, 2, 3, 4, 2],
                  [0, 1, 0, 2, 2],
                  [0, 0, 0, 3, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm2' (createLevelMap lv2') `shouldBe` False

      let pm4 = createPlayerMap [[ri 1 1, ri 1 5], [ri 5 1, ri 5 2]]
      let lv4 =
            Map.fromList $
              (zip validIndices . concat)
                [ [1, 2, 3, 2, 2],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm4 (createLevelMap lv4) `shouldBe` False

      let pm8 = createPlayerMap [[ri 2 2, ri 5 1], [ri 5 4, ri 5 5]]
      let lv8 =
            Map.fromList $
              (zip validIndices . concat)
                [ [0, 1, 2, 3, 0],
                  [3, 0, 0, 0, 0],
                  [0, 2, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm8 (createLevelMap lv8) `shouldBe` False

      -- yes instances
      let pm3 = createPlayerMap [[ri 1 1, ri 1 5], [ri 5 1, ri 5 2]]
      let lv3 =
            Map.fromList $
              (zip validIndices . concat)
                [ [1, 2, 3, 4, 3],
                  [0, 0, 3, 4, 2],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm3 (createLevelMap lv3) `shouldBe` True

      let pm5 = createPlayerMap [[ri 1 1, ri 1 5], [ri 5 1, ri 5 2]]
      let lv5 =
            Map.fromList $
              (zip validIndices . concat)
                [ [3, 2, 0, 2, 1],
                  [0, 3, 0, 3, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm5 (createLevelMap lv5) `shouldBe` True

      let pm6 = createPlayerMap [[ri 1 1, ri 1 3], [ri 5 1, ri 5 2]]
      let lv6 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 2, 3, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm6 (createLevelMap lv6) `shouldBe` True

      let pm7 = createPlayerMap [[ri 1 1, ri 1 5], [ri 1 2, ri 2 4]]
      let lv7 =
            Map.fromList $
              (zip validIndices . concat)
                [ [3, 2, 3, 4, 2],
                  [0, 2, 0, 2, 2],
                  [0, 0, 0, 3, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Artemis) 0 pm7 (createLevelMap lv7) `shouldBe` True

    it "works with Minotaur" $ do
      -- no instances
      let pm1 = createPlayerMap [[ri 1 1, ri 1 5], [ri 1 2, ri 1 3]]
      let lv1 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 0, 3, 2],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm1 (createLevelMap lv1) `shouldBe` False

      let pm2 = createPlayerMap [[ri 1 1, ri 1 2], [ri 5 2, ri 5 3]]
      let lv2 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 0, 0, 0],
                  [0, 3, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm2 (createLevelMap lv2) `shouldBe` False

      let pm3 = createPlayerMap [[ri 1 1, ri 1 3], [ri 1 2, ri 5 3]]
      let lv3 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 2, 3, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm3 (createLevelMap lv3) `shouldBe` False

      let pm8 = createPlayerMap [[ri 1 1, ri 1 3], [ri 2 2, ri 5 3]]
      let lv8 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 0, 2, 0, 0],
                  [0, 3, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm8 (createLevelMap lv8) `shouldBe` False

      -- yes instances
      let pm4 = createPlayerMap [[ri 1 1, ri 1 5], [ri 1 3, ri 5 3]]
      let lv4 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 0, 3, 2],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm4 (createLevelMap lv4) `shouldBe` True

      let pm5 = createPlayerMap [[ri 1 1, ri 1 5], [ri 1 2, ri 1 4]]
      hasDoubleLizhi (Just Minotaur) 0 pm5 (createLevelMap lv4) `shouldBe` True

      let pm6 = createPlayerMap [[ri 1 1, ri 1 2], [ri 5 2, ri 5 3]]
      let lv6 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 0, 0, 0],
                  [3, 3, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm6 (createLevelMap lv6) `shouldBe` True

      let pm7 = createPlayerMap [[ri 1 1, ri 1 3], [ri 2 2, ri 5 3]]
      let lv7 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 0, 2, 3, 0],
                  [0, 3, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi (Just Minotaur) 0 pm7 (createLevelMap lv7) `shouldBe` True

    it "works with others" $ do
      let pm1 = createPlayerMap [[ri 1 1, ri 1 3], [ri 5 1, ri 5 2]]
      let lv1 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 2, 2, 2],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi Nothing 0 pm1 (createLevelMap lv1) `shouldBe` False

      let pm2 = createPlayerMap [[ri 1 1, ri 1 4], [ri 5 1, ri 5 2]]
      let lv2 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 1, 2, 3],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi Nothing 0 pm2 (createLevelMap lv2) `shouldBe` True

      let pm3 = createPlayerMap [[ri 1 1, ri 1 4], [ri 5 1, ri 5 2]]
      let lv3 =
            Map.fromList $
              (zip validIndices . concat)
                [ [2, 3, 1, 2, 2],
                  [0, 3, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0]
                ]
      hasDoubleLizhi Nothing 0 pm3 (createLevelMap lv3) `shouldBe` True

  describe "Evaluation#hasOneXiangting()" $ do
    let commonYesInstances =
          [ ( [[ri 1 1, ri 2 1], [ri 5 1, ri 5 2]],
              [[2, 2, 3, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 2 2, ri 2 1], [ri 5 1, ri 5 2]],
              [[2, 2, 3, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 1 3, ri 2 1], [ri 5 1, ri 5 2]],
              [[2, 2, 3, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 2 2, ri 1 1], [ri 5 1, ri 5 2]],
              [[2, 2, 3, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 1 3, ri 1 1], [ri 5 1, ri 5 2]],
              [[2, 2, 3, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 1 3, ri 3 3], [ri 5 3, ri 5 2]],
              [[3, 0, 1, 0, 3], [0, 2, 0, 2, 0], [0, 0, 1, 0, 0], [0, 2, 0, 2, 0], [3, 0, 0, 2, 3]]
            ),
            ( [[ri 4 5, ri 3 3], [ri 1 2, ri 1 3]],
              [[0, 0, 0, 0, 0], [0, 0, 0, 2, 0], [0, 0, 1, 4, 1], [0, 0, 3, 4, 2], [0, 0, 4, 2, 4]]
            ),
            -- cooperation
            ( [[ri 1 1, ri 1 2], [ri 5 1, ri 5 2]],
              [[2, 3, 0, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 1 2, ri 1 3], [ri 5 1, ri 5 2]],
              [[2, 2, 3, 0, 0], [2, 2, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 1 1, ri 2 2], [ri 5 1, ri 5 2]],
              [[3, 2, 0, 0, 0], [2, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 2 1, ri 3 2], [ri 5 1, ri 5 2]],
              [[4, 4, 3, 0, 0], [2, 2, 4, 0, 0], [4, 3, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 3 1, ri 1 1], [ri 5 1, ri 5 2]],
              [[3, 2, 3, 0, 0], [0, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            ),
            ( [[ri 3 1, ri 1 1], [ri 5 1, ri 5 2]],
              [[3, 2, 3, 0, 0], [0, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
            )
          ]
    let check c pl lv expected =
          let lv' = Map.fromList ((zip validIndices . concat) lv)
           in do
                -- putStr $ "checking: " ++ show pl ++ " " ++ show lv ++ "\n"
                let pl' = [pl !! 1, pl !! 0]
                hasOneXiangting c 0 pl (createPlayerMap pl) lv' (createLevelMap lv') `shouldBe` expected
                hasOneXiangting c 1 pl' (createPlayerMap pl') lv' (createLevelMap lv') `shouldBe` expected

    it "works with common instances" $ do
      check
        Nothing
        [[ri 3 1, ri 1 1], [ri 5 1, ri 2 1]]
        [[3, 2, 3, 0, 0], [0, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False

      forM_ commonYesInstances $ \(pm, lv) ->
        let cs = [Nothing, Just Hephastus, Just Pan, Just Demeter, Just Prometheus, Just Apollo, Just Minotaur, Just Atlas, Just Artemis]
         in forM_ cs $ \c ->
              check c pm lv True

    it "works with Prometheus" $ do
      let c = Just Prometheus
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[2, 2, 0, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[1, 2, 2, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[1, 2, 3, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[3, 2, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[3, 1, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[3, 1, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 2 2], [ri 2 3, ri 5 2]]
        [[3, 1, 3, 0, 0], [4, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[2, 1, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check -- best not to make double builds
        c
        [[ri 1 1, ri 2 2], [ri 2 3, ri 5 2]]
        [[3, 2, 2, 0, 0], [4, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      -- cooperation
      check
        c
        [[ri 1 1, ri 1 2], [ri 5 1, ri 5 2]]
        [[2, 3, 0, 0, 0], [1, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 1 2], [ri 5 1, ri 2 2]]
        [[2, 3, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 2, ri 3 2], [ri 5 1, ri 5 2]]
        [[0, 2, 2, 0, 0], [2, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      -- make sure subroutines are correct
      let xpl = [[ri 1 2, ri 3 2], [ri 5 1, ri 2 3]]
      let xlv = Map.fromList ((zip validIndices . concat) [[0, 2, 2, 0, 0], [2, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
      hasOneXiangtingPrometheus' 0 xpl (createPlayerMap xpl) xlv (createLevelMap xlv) 1 `shouldBe` False

      check
        c
        [[ri 1 2, ri 3 2], [ri 5 1, ri 2 3]]
        [[0, 2, 2, 0, 0], [2, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 2, ri 3 2], [ri 5 1, ri 5 2]]
        [[0, 2, 2, 0, 0], [2, 1, 1, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 2, ri 2 2], [ri 5 1, ri 5 2]]
        [[2, 2, 0, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 3 2], [ri 2 2, ri 3 3]]
        [[0, 2, 0, 0, 0], [2, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 3 2], [ri 2 2, ri 3 1]]
        [[0, 2, 0, 0, 0], [2, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 2 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[0, 4, 0, 0, 0], [2, 2, 0, 0, 0], [4, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      -- make sure subroutines are correct
      let ypl = [[ri 2 1, ri 3 2], [ri 1 1, ri 5 2]]
      let ylv = Map.fromList ((zip validIndices . concat) [[0, 4, 0, 0, 0], [2, 2, 4, 0, 0], [4, 2, 4, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0]])
      hasOneXiangtingPrometheus' 0 ypl (createPlayerMap ypl) ylv (createLevelMap ylv) 0 `shouldBe` False
      hasOneXiangtingPrometheus' 0 ypl (createPlayerMap ypl) ylv (createLevelMap ylv) 1 `shouldBe` False

      check
        c
        [[ri 2 1, ri 3 2], [ri 1 1, ri 5 2]]
        [[0, 4, 0, 0, 0], [2, 2, 4, 0, 0], [4, 2, 4, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0]]
        False

    it "works with Apollo" $ do
      let c = Just Apollo
      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[1, 2, 3, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[2, 2, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[3, 2, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[3, 2, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 2, ri 2 2]]
        [[2, 2, 3, 0, 0], [0, 0, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 2, ri 2 2]]
        [[2, 2, 3, 0, 0], [0, 0, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      -- cooperation
      check
        c
        [[ri 1 1, ri 1 2], [ri 5 1, ri 2 2]]
        [[3, 2, 0, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

    it "works with Minotaur" $ do
      let c = Just Minotaur

      -- make sure subroutines are correct
      let xpl = [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
      let xpl' = [[ri 1 2, ri 5 2], [ri 1 1, ri 3 2]]
      let xlv = Map.fromList ((zip validIndices . concat) [[1, 2, 3, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])

      -- print $ getLegalMoveTo (Just Minotaur) (ri 1 1) (createPlayerMap xpl') xlv (createLevelMap xlv)
      -- print $ getLegalPushTo (Just Minotaur) xpl' (ri 1 1) (ri 1 2)
      -- print xpl'
      -- let xpl'' = [[if pp == 1 && 0 == ww then 9 else if pp /= 1 && 0 == ww then 10 else xpl' !! pp !! ww | ww <- [0, 1]] | pp <- [0, 1]]
      -- print xpl''
      -- print $ getLegalMoveTo (Just Minotaur) (ri 1 2) (createPlayerMap xpl'') xlv (createLevelMap xlv)

      hasOneXiangtingMinotaur'' 0 xpl (createPlayerMap xpl) xlv (createLevelMap xlv) 0 `shouldBe` True
      hasOneXiangtingMinotaur'' 1 xpl' (createPlayerMap xpl') xlv (createLevelMap xlv) 0 `shouldBe` True

      let ypl = [[ri 1 1, ri 1 4], [ri 1 2, ri 5 2]]
      let ypl' = [[ri 1 2, ri 5 2], [ri 1 1, ri 1 4]]
      let ylv = Map.fromList ((zip validIndices . concat) [[1, 2, 3, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])

      -- print $ getLegalMoveTo (Just Minotaur) (ri 1 1) (createPlayerMap ypl') ylv (createLevelMap ylv)
      -- print $ getLegalPushTo (Just Minotaur) ypl' (ri 1 1) (ri 1 2)
      -- print ypl'
      -- let ypl'' = [[if pp == 1 && 0 == ww then 9 else if pp /= 1 && 0 == ww then 10 else ypl' !! pp !! ww | ww <- [0, 1]] | pp <- [0, 1]]
      -- print ypl''
      -- print $ getLegalMoveTo (Just Minotaur) (ri 1 2) (createPlayerMap ypl'') ylv (createLevelMap ylv)

      hasOneXiangtingMinotaur'' 0 ypl (createPlayerMap ypl) ylv (createLevelMap ylv) 0 `shouldBe` False
      hasOneXiangtingMinotaur'' 1 ypl' (createPlayerMap ypl') xlv (createLevelMap ylv) 0 `shouldBe` False

      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[1, 2, 3, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 1 4], [ri 1 2, ri 5 2]]
        [[1, 2, 3, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[3, 2, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[3, 2, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      check
        c
        [[ri 1 1, ri 3 2], [ri 1 2, ri 5 2]]
        [[3, 2, 3, 4, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 2, ri 2 2]]
        [[3, 2, 3, 0, 0], [0, 0, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False

      let zpl = [[ri 1 1, ri 2 1], [ri 5 2, ri 2 2]]
      let zlv = Map.fromList ((zip validIndices . concat) [[3, 2, 3, 0, 0], [0, 0, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])

      -- print $ getLegalMoveTo (Just Minotaur) (ri 1 1) (createPlayerMap zpl) zlv (createLevelMap zlv)
      -- print $ getLegalPushTo (Just Minotaur) zpl (ri 1 1) (ri 1 2)
      -- print zpl
      -- let zpl'' = [[if pp == 0 && 0 == ww then 9 else zpl !! pp !! ww | ww <- [0, 1]] | pp <- [0, 1]]
      -- print zpl''
      -- print $ getLegalMoveTo (Just Minotaur) (ri 1 2) (createPlayerMap zpl'') xlv (createLevelMap zlv)

      hasOneXiangtingMinotaur'' 0 zpl (createPlayerMap zpl) zlv (createLevelMap zlv) 0 `shouldBe` False

      check
        c
        [[ri 1 1, ri 2 1], [ri 5 2, ri 2 2]]
        [[3, 2, 3, 0, 0], [0, 0, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 2 3], [ri 1 2, ri 2 2]]
        [[3, 2, 3, 0, 0], [3, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 5 3], [ri 1 2, ri 5 2]]
        [[1, 2, 2, 0, 0], [0, 0, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      -- cooperation
      check
        c
        [[ri 1 1, ri 1 2], [ri 2 2, ri 5 2]]
        [[3, 2, 0, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 1 2], [ri 2 2, ri 3 3]]
        [[3, 2, 0, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False

    it "works with Demeter" $ do
      let c = Just Demeter
      check
        c
        [[ri 1 1, ri 3 2], [ri 5 1, ri 5 2]]
        [[2, 2, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      -- cooperation
      check
        c
        [[ri 1 2, ri 3 2], [ri 5 1, ri 5 2]]
        [[2, 2, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 3 2], [ri 5 1, ri 2 2]]
        [[2, 2, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 2, ri 1 3], [ri 5 1, ri 5 2]]
        [[2, 2, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 1 3], [ri 5 1, ri 5 2]]
        [[2, 2, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 1 3], [ri 5 1, ri 2 2]]
        [[2, 2, 3, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 3 2, ri 3 3], [ri 2 3, ri 3 1]]
        [[0, 0, 0, 0, 0], [0, 2, 0, 0, 0], [0, 0, 2, 2, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 4 2], [ri 5 1, ri 5 2]]
        [[0, 2, 0, 0, 0], [2, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 3, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 2 2], [ri 5 1, ri 5 2]]
        [[0, 2, 0, 0, 0], [2, 1, 2, 0, 0], [0, 4, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False

    it "works with Hephastus" $ do
      let c = Just Hephastus
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 5 2]]
        [[1, 2, 3, 0, 0], [1, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 3 3, ri 3 1], [ri 5 1, ri 5 2]]
        [[0, 4, 4, 1, 0], [0, 4, 2, 4, 0], [0, 4, 3, 4, 0], [0, 0, 4, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 1 1], [ri 5 1, ri 5 2]]
        [[3, 2, 1, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 2, ri 1 1], [ri 5 1, ri 2 2]]
        [[3, 2, 1, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 3 1, ri 1 1], [ri 5 1, ri 5 2]]
        [[3, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False

    it "works with Artemis" $ do
      let c = Just Artemis
      check
        c
        [[ri 1 1, ri 5 1], [ri 5 4, ri 5 5]]
        [[0, 1, 2, 3, 0], [0, 2, 0, 0, 0], [0, 3, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 5 1], [ri 5 4, ri 5 5]]
        [[0, 1, 1, 3, 0], [0, 2, 0, 0, 0], [0, 3, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 5 1], [ri 5 4, ri 5 5]]
        [[1, 2, 0, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 3, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      -- make sure subroutines are correct
      let xpl = [[ri 1 1, ri 5 1], [ri 5 4, ri 5 5]]
      let xpm = createPlayerMap xpl
      let xlv = Map.fromList ((zip validIndices . concat) [[0, 1, 2, 3, 0], [2, 0, 0, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]])
      let xlm = createLevelMap xlv
      -- print $ getLegalMoveTo (Just Artemis) (ri 1 1) xpm xlv xlm

      -- let xmt = getLegalMoveTo (Just Artemis) (ri 1 1) xpm xlv xlm !! 3
      --     xpm' = foldl' (flip (Map.adjust (xor (listToBB [(ri 1 1), xmt])))) xpm [2 * 0 + 0, 4 + 0, 6]
      --     buildAt = getNeighborhood xmt .&. (xlm ! 7) `andNotBB` (xpm' ! 6)
      --  in do
      --       print $ xpm'
      --       print $ bbToList buildAt
      --       print
      --         [ hasDoubleLizhi (Just Artemis) 0 xpm' (makeNextLevelMap xlm [(ba, xlv ! ba, (xlv ! ba) + 1)])
      --           | ba <- (bbToList buildAt)
      --         ]
      hasOneXiangtingArtemis 0 xpl xpm xlv xlm 0 `shouldBe` False

      check
        c
        [[ri 1 1, ri 5 1], [ri 5 4, ri 5 5]]
        [[0, 1, 2, 3, 0], [2, 0, 0, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 3 2, ri 5 1], [ri 5 4, ri 5 5]]
        [[0, 1, 2, 3, 0], [2, 0, 0, 0, 0], [0, 3, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 3 2, ri 5 1], [ri 5 4, ri 5 5]]
        [[4, 1, 2, 3, 0], [2, 4, 4, 0, 0], [0, 3, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 3 2, ri 2 3], [ri 5 4, ri 5 5]]
        [[4, 1, 2, 3, 0], [2, 4, 0, 0, 0], [0, 3, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 3 2, ri 5 1], [ri 2 3, ri 5 5]]
        [[4, 1, 2, 3, 0], [2, 4, 4, 0, 0], [0, 3, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 3 2, ri 5 1], [ri 2 3, ri 5 5]]
        [[0, 1, 2, 3, 0], [2, 0, 0, 0, 2], [0, 3, 2, 1, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

      -- cooperation (both workers have one lizhi)
      check
        c
        [[ri 1 1, ri 1 3], [ri 5 4, ri 5 5]]
        [[3, 2, 1, 0, 0], [0, 0, 0, 0, 0], [1, 0, 0, 0, 0], [2, 0, 0, 0, 0], [3, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 1 3], [ri 5 4, ri 5 5]]
        [[3, 2, 1, 0, 0], [1, 0, 0, 0, 0], [2, 0, 0, 0, 0], [2, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 1 3], [ri 5 4, ri 5 5]]
        [[3, 2, 1, 0, 0], [0, 0, 0, 0, 0], [1, 0, 0, 0, 0], [1, 0, 0, 0, 0], [3, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 4, ri 2 2], [ri 5 4, ri 5 5]]
        [[0, 0, 2, 1, 0], [0, 3, 0, 0, 0], [2, 0, 0, 0, 0], [1, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check -- move and build lv 2 -> 3
        c
        [[ri 1 1, ri 3 1], [ri 5 4, ri 5 5]]
        [[0, 0, 2, 3, 0], [0, 1, 0, 0, 0], [1, 0, 2, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check -- move and build lv 1 -> 2
        c
        [[ri 1 1, ri 4 2], [ri 5 4, ri 5 5]]
        [[0, 0, 2, 3, 0], [0, 1, 0, 0, 0], [0, 0, 1, 3, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 4 2], [ri 5 4, ri 5 5]]
        [[0, 0, 2, 0, 0], [0, 1, 0, 3, 0], [0, 0, 1, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0]]
        False

      -- cooperation (one worker has two lizhi's)
      check -- build lv 2 -> 3
        c
        [[ri 1 1, ri 1 3], [ri 5 4, ri 5 5]]
        [[3, 2, 1, 0, 0], [0, 0, 2, 0, 0], [0, 0, 2, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check -- build lv 1 -> 2
        c
        [[ri 1 1, ri 1 3], [ri 5 4, ri 5 5]]
        [[3, 2, 1, 0, 0], [0, 0, 0, 1, 0], [0, 0, 0, 3, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check -- build lv 1 -> 2
        c
        [[ri 1 1, ri 2 1], [ri 5 4, ri 5 5]]
        [[0, 0, 1, 0, 0], [3, 0, 1, 0, 0], [0, 3, 0, 3, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True

    it "works with Pan" $ do
      let c = Just Pan
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 5 2]]
        [[2, 2, 3, 0, 0], [1, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 5 2]]
        [[2, 2, 0, 0, 0], [1, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 5 2]]
        [[3, 2, 0, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 5 2]]
        [[3, 2, 2, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 2 4, ri 3 5], [ri 5 1, ri 5 2]]
        [[4, 4, 4, 4, 4], [4, 4, 4, 3, 1], [0, 4, 2, 1, 2], [0, 4, 4, 1, 1], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 2 4, ri 3 5], [ri 5 1, ri 5 2]]
        [[4, 4, 4, 4, 4], [4, 4, 4, 3, 1], [0, 4, 2, 1, 2], [0, 0, 4, 1, 1], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 5 2]]
        [[2, 2, 0, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 3 1], [ri 5 1, ri 1 3]]
        [[2, 2, 0, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 3, ri 2 1]]
        [[2, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 3, ri 2 2]]
        [[2, 2, 0, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 2, ri 2 2]]
        [[2, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 1, ri 2 1], [ri 1 3, ri 2 2]]
        [[3, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 2, ri 2 1], [ri 5 1, ri 5 2]]
        [[4, 2, 4, 1, 0], [0, 1, 2, 1, 0], [1, 1, 1, 1, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        True
      -- should not move from lv 0 to lv 2
      check
        c
        [[ri 1 2, ri 2 1], [ri 5 1, ri 5 2]]
        [[4, 2, 4, 1, 0], [0, 4, 2, 1, 0], [1, 2, 1, 1, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        c
        [[ri 1 3, ri 2 2], [ri 5 1, ri 5 2]]
        [[4, 4, 2, 4, 4], [0, 0, 4, 2, 4], [4, 4, 4, 4, 4], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      -- has to build at lv 3 to 4
      check
        c
        [[ri 3 1, ri 1 1], [ri 5 1, ri 5 2]]
        [[3, 2, 3, 0, 0], [4, 4, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
    it "works with others" $ do
      check
        Nothing
        [[ri 1 3, ri 1 1], [ri 5 1, ri 5 2]]
        [[2, 2, 3, 0, 0], [0, 1, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        Nothing
        [[ri 1 3, ri 3 3], [ri 5 3, ri 5 2]]
        [[3, 0, 1, 0, 3], [0, 2, 0, 2, 0], [0, 0, 1, 0, 0], [0, 2, 0, 2, 0], [3, 0, 0, 0, 3]]
        False
      check
        Nothing
        [[ri 2 1, ri 1 1], [ri 5 1, ri 1 2]]
        [[2, 2, 3, 0, 0], [2, 0, 2, 0, 0], [0, 2, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        Nothing
        [[ri 1 1, ri 1 2], [ri 5 1, ri 5 2]]
        [[2, 3, 0, 0, 0], [2, 4, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        Nothing
        [[ri 1 1, ri 2 2], [ri 5 1, ri 5 2]]
        [[2, 2, 4, 0, 0], [4, 3, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
      check
        Nothing
        [[ri 1 1, ri 2 2], [ri 5 1, ri 1 3]]
        [[2, 2, 0, 0, 0], [4, 3, 4, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]
        False
