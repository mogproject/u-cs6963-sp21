module Game.BitBoardSpec (spec) where

import qualified Data.Bits as Bits
import Game.BitBoard
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "BitBoard#getClosedNeighborhood()" $ do
    prop "is correct" $ \x ->
      let bb = x Bits..&. globalMask
          expected = foldl (\a i -> a Bits..|. getNeighborhood i) bb (bbToList bb)
       in getClosedNeighborhood bb `shouldBe` expected

  describe "BitBoard#listToBB()" $ do
    prop "is inverse to bbToList()" $ \x ->
      let bb = x Bits..&. globalMask
       in (listToBB . bbToList) bb `shouldBe` bb

  describe "BitBoard#countBB()" $ do
    prop "is correct" $ \x ->
      let bb = x Bits..&. globalMask
       in countBB bb `shouldBe` length (bbToList bb)
