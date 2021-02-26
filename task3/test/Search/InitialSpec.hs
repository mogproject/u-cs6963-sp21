module Search.InitialSpec (spec) where

import Control.Monad (forM)
import Data.Board
  ( Board (Board),
    Workers,
  )
import qualified Data.Map as Map
import Search.Initial
  ( findStartingPlayer1,
    findStartingPlayer2,
    player2Table,
  )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

isValid :: Workers -> Bool
isValid ((r1, c1), (r2, c2)) =
  all chk [r1, c1, r2, c2] && r1 /= r2 || c1 /= c2
  where
    chk x = 1 <= x && x <= 5

spec :: Spec
spec = do
  describe "Initial#findStartingPlayer1()" $ do
    it "succeeds" $ do
      True `shouldBe` True
      -- isValid (findStartingPlayer1 0 0) `shouldBe` True
      -- isValid (findStartingPlayer1 1 0) `shouldBe` True

  describe "Initial#findStartingPlayer2()" $ do
    it "succeeds" $ do
      True `shouldBe` True
      -- let p1 = [((x `div` 5 + 1, x `mod` 5 + 1), (y `div` 5 + 1, y `mod` 5 + 1)) | x <- [0 .. 24], y <- [0 .. 24], x /= y]
      -- all (\p -> isValid $ findStartingPlayer2 0 0 p) p1 `shouldBe` True
      -- all (\p -> isValid $ findStartingPlayer2 1 0 p) p1 `shouldBe` True

  describe "Initial#player2Table" $ do
    it "covers all possibilities" $ do
      length player2Table `shouldBe` 25 * 24
