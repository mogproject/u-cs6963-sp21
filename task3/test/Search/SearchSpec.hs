module Search.SearchSpec (spec) where

import Data.Board (readBoard)
import Data.Maybe (fromMaybe)
import Game.GameState (fromBoard)
import Search.Search
import Test.Hspec

spec :: Spec
spec = do
  describe "Evaluation#findMove()" $ do
    it "does not crash" $ do
      let s61 = "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,2],[3,4]]},{\"card\":\"Apollo\",\"tokens\":[[1,3],[4,2]]}],\"spaces\":[[2,4,1,4,4],[4,2,4,2,2],[2,4,2,2,2],[2,2,4,4,4],[4,4,4,1,0]],\"turn\":70}"
      let b61 = fromBoard $ fromMaybe undefined (readBoard s61)

      findMove 3 0 b61 `shouldBe` 9223369781996962572
