module Game.GameStateSpec (spec) where

import Data.Int (Int64)
import Game.GameMove
import Game.GameState
import Test.Hspec
import Data.Maybe (fromMaybe)
import Data.Board (readBoard)

spec :: Spec
spec = do
  describe "GameState#getLegalMoves()" $ do
    it "is inverse to setWorkerId()" $ do
      let s1 = "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,2],[3,4]]},{\"card\":\"Apollo\",\"tokens\":[[1,3],[4,2]]}],\"spaces\":[[2,4,1,4,4],[4,2,4,2,2],[2,4,2,2,2],[2,2,4,4,4],[4,4,4,1,0]],\"turn\":70}"
      let st = fromBoard $ fromMaybe undefined (readBoard s1)
      (length . legalMoves) st `shouldBe` 21
