module Game.EvaluationSpec (spec) where

-- Board (Board),
-- Players,
-- choosePlayers,
-- players,

-- readPlayers,
-- spaces,
-- turn,

-- writePlayers,

--  GameMove,
-- GameState (GameState),
-- evaluate,

-- levels,

-- players,
-- toBoard,
-- turn,`

import Control.Monad
import Data.Board
  ( readBoard,
    writeBoard,
  )
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Game.Evaluation
  ( evaluate,
    evaluateAsymmetry',
    evaluatePrevention',
    evaluateReachability',
    evaluateStuckBonus',
    evaluateWorkerProximity',
  )
import Game.GameState
  ( decodeMove,
    fromBoard,
    legalMoves,
    makeMove,
    toBoard,
  )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

eval :: String -> Int
eval s = evaluate $ fromBoard (fromMaybe undefined (readBoard s))

printInfo :: String -> IO ()
printInfo s =
  let b = fromBoard (fromMaybe undefined (readBoard s))
      fs = [evaluateAsymmetry', evaluatePrevention', evaluateReachability', evaluateStuckBonus', evaluateWorkerProximity']
   in print [[f b p | f <- fs] | p <- [0, 1]]

spec :: Spec
spec = do
  describe "Evaluation#evaluate()" $ do
    it "finds out end game" $ do
      let s11 = "{\"turn\":8,\"spaces\":[[0,0,2,0,0],[0,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[1,4],[1,5]],[[3,3],[3,4]]]}"
      let s12 = "{\"turn\":9,\"spaces\":[[0,0,2,0,0],[1,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[1,4],[1,5]],[[3,3],[3,4]]]}"

      eval s11 `shouldBe` -1000000000
      eval s12 `shouldBe` -1000000000

    it "prefers building farther from opponent" $ do
      let s1 = "{\"turn\":1,\"spaces\":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,5],[5,5]],[[1,2],[2,2]]]}"
      let s2 = "{\"turn\":1,\"spaces\":[[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,5],[5,5]],[[1,2],[2,2]]]}"
      let s3 = "{\"turn\":1,\"spaces\":[[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,5],[5,5]],[[2,1],[2,2]]]}"

      eval s1 `shouldBe` -256
      eval s2 `shouldBe` -203
      eval s3 `shouldBe` -181

    it "prefers building next to the boundary" $ do
      let s21 = "{\"turn\":3,\"spaces\":[[1,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1]],\"players\":[[[4,4],[4,5]],[[1,1],[2,2]]]}"
      let s22 = "{\"turn\":3,\"spaces\":[[1,0,0,0,0],[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1]],\"players\":[[[4,4],[4,5]],[[2,2],[3,3]]]}"

      let b21 = fromBoard (fromMaybe undefined (readBoard s21))
      let b22 = fromBoard (fromMaybe undefined (readBoard s22))

      evaluateWorkerProximity' b21 0 `shouldBe` 200
      evaluateWorkerProximity' b21 1 `shouldBe` 200
      evaluateWorkerProximity' b22 0 `shouldBe` 200
      evaluateWorkerProximity' b22 1 `shouldBe` 200

      evaluateReachability' b21 0 `shouldBe` 401
      evaluateReachability' b21 1 `shouldBe` 581
      evaluateReachability' b22 0 `shouldBe` 389
      evaluateReachability' b22 1 `shouldBe` 602

      evaluateAsymmetry' b21 0 `shouldBe` 170
      evaluateAsymmetry' b21 1 `shouldBe` 330
      evaluateAsymmetry' b22 0 `shouldBe` 160
      evaluateAsymmetry' b22 1 `shouldBe` 260

      -- s21 is better for Player1
      evaluate b21 < evaluate b22 `shouldBe` True
      evaluate b21 `shouldBe` -340
      evaluate b22 `shouldBe` -313

    it "prefers preventing opponent from climbing up" $ do
      let s31 = "{\"turn\":2,\"spaces\":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,1,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[2,1],[2,2]],[[2,3],[3,3]]]}"
      let s32 = "{\"turn\":2,\"spaces\":[[2,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[2,1],[2,2]],[[1,2],[3,2]]]}"

      eval s31 `shouldBe` 174
      eval s32 `shouldBe` 33

    it "prefers climbing up" $ do
      let s41 = "{\"players\":[[[3,2],[4,1]],[[4,4],[4,5]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,2]],\"turn\":3}"
      let s42 = "{\"players\":[[[3,2],[4,1]],[[4,4],[5,5]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,1],[0,0,0,0,1]],\"turn\":3}"
      -- printInfo s41
      -- printInfo s42

      -- s42 is better for Player 1
      eval s41 > eval s42 `shouldBe` True

      let s43 = "{\"players\":[[[2,2],[4,2]],[[4,4],[4,5]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,2]],\"turn\":3}"
      let s44 = "{\"players\":[[[2,2],[4,2]],[[4,4],[5,5]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1],[0,0,1,0,1]],\"turn\":3}"

      -- eval s44 `shouldBe` -2005

      -- printInfo s43
      -- printInfo s44
      eval s44 < eval s43 `shouldBe` True

    it "works with several initial positions" $ do
      let s51 = "{\"turn\":0,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,4],[5,5]],[[1,1],[2,2]]]}"
      let b51 = fromBoard (fromMaybe undefined (readBoard s51))

      let nextStates = [(m, makeMove b51 m) | m <- legalMoves b51]
      let nextMove = minimumBy (comparing fst) [(evaluate st, m) | (m, st) <- nextStates]

      -- print $ sortBy (comparing fst) [(evaluate st, m) | (m, st) <- nextStates]

      let s52 = "{\"players\":[[[4,4],[5,4]],[[2,2],[4,2]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,1]],\"turn\":2}"
      let b52 = fromBoard (fromMaybe undefined (readBoard s52))
      -- print [((-1) * evaluate (makeMove b52 m), m) | m <- legalMoves b52]
      -- print $ maximumBy (comparing fst) [((-1) * evaluate (makeMove b52 m), m) | m <- legalMoves b52]
      let bestMove = snd $ maximumBy (comparing fst) [((-1) * evaluate (makeMove b52 m), m) | m <- legalMoves b52]
      bestMove `elem` legalMoves b52 `shouldBe` True

      -- print (evaluate (makeMove b52 bestMove))
      -- print $ writeBoard (toBoard (makeMove b52 bestMove))

    it "makeMove should not affect the evaluation" $ do
      let s52 = "{\"players\":[[[4,4],[5,4]],[[2,2],[4,2]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,1]],\"turn\":2}"
      let b52 = fromBoard (fromMaybe undefined (readBoard s52))
      let b52' = makeMove b52 50415

      let s53 = "{\"players\":[[[2,2],[4,2]],[[4,4],[4,5]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,2]],\"turn\":3}"
      let b53 = fromBoard (fromMaybe undefined (readBoard s53))

      toBoard b52' `shouldBe` toBoard b53
      b52' `shouldBe` b53
      eval s53 `shouldBe` evaluate b52'

  describe "GameState#makeMove()" $ do
    prop "does not have side effects" $ \b ->
      let state = fromBoard b
          moves = legalMoves state
          nextStates = [makeMove state m | m <- moves]
       in -- do
          --  print $ head moves
          --  (fromBoard . toBoard) (head nextStates) `shouldBe` (head nextStates)
          foldl
            (\_ x -> (fromBoard . toBoard) x `shouldBe` x)
            (True `shouldBe` True)
            nextStates
