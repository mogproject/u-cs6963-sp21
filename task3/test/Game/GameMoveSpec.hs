module Game.GameMoveSpec (spec) where

import Data.Int (Int64)
import Game.GameMove
import Test.Hspec

spec :: Spec
spec = do
  describe "GameMove#getWorkerId()" $ do
    it "is inverse to setWorkerId()" $ do
      let x = createGameMove
      setWorkerId 0 x `shouldBe` x
      setWorkerId 1 x `shouldBe` x + 1
      getWorkerId x `shouldBe` 0
      getWorkerId x + 1 `shouldBe` 1

  describe "GameMove#getBuildAt()" $ do
    it "is inverse to setBuildAt()" $ do
      let x = createGameMove
      -- print x
      -- print $ setBuildAt [(24, 2)] x
      getBuildAt (-1 :: Int64) `shouldBe` []
      getBuildAt x `shouldBe` []
      (getBuildAt . setBuildAt [(24, 2)]) x `shouldBe` [(24, 2)]
