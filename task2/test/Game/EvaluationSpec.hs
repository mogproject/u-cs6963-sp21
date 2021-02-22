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
import Game.Evaluation (evaluate)
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

spec :: Spec
spec = do
  describe "GameState#score()" $ do
    it "finds out end game" $ do
      let s11 = "{\"turn\":8,\"spaces\":[[0,0,2,0,0],[0,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[1,4],[1,5]],[[3,3],[3,4]]]}"
      let s12 = "{\"turn\":9,\"spaces\":[[0,0,2,0,0],[1,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[1,4],[1,5]],[[3,3],[3,4]]]}"

      let b11 = fromBoard (fromMaybe undefined (readBoard s11))
      let b12 = fromBoard (fromMaybe undefined (readBoard s12))

      evaluate b11 `shouldBe` -1000000000
      evaluate b12 `shouldBe` -1000000000

    it "prefers building farther from opponent" $ do
      let s1 = "{\"turn\":1,\"spaces\":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,5],[5,5]],[[1,2],[2,2]]]}"
      let s2 = "{\"turn\":1,\"spaces\":[[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,5],[5,5]],[[1,2],[2,2]]]}"
      let s3 = "{\"turn\":1,\"spaces\":[[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],\"players\":[[[4,5],[5,5]],[[2,1],[2,2]]]}"
      let b1 = fromBoard (fromMaybe undefined (readBoard s1))
      let b2 = fromBoard (fromMaybe undefined (readBoard s2))
      let b3 = fromBoard (fromMaybe undefined (readBoard s3))

      evaluate b1 `shouldBe` -256
      evaluate b2 `shouldBe` -203
      evaluate b3 `shouldBe` -181
