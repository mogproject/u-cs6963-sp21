module Game.EvaluationSpec (spec) where

import Data.Board
  ( -- Board (Board),
    -- Players,
    -- choosePlayers,
    -- players,
    readBoard,
    -- readPlayers,
    -- spaces,
    -- turn,
    -- writeBoard,
    -- writePlayers,
  )
import Data.Maybe (fromMaybe)
import Game.Evaluation
  ( evaluate,
    evaluateAsymmetry',
    evaluatePrevention',
    evaluateReachability',
    evaluateStuckBonus',
    evaluateWorkerProximity',
  )
import Game.GameState
  ( --  GameMove,
    -- GameState (GameState),
    -- evaluate,
    fromBoard,
    -- legalMoves,
    -- levels,
    -- makeMove,
    -- players,
    -- toBoard,
    -- turn,`
  )
import Test.Hspec

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
