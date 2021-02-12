module Data.BoardSpec (spec) where

import Data.Board
  ( Board (Board),
    players,
    readBoard,
    readPlayers,
    spaces,
    turn,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "Board#readBoard()" $ do
    it "fails with invalid input" $ do
      readBoard "" `shouldBe` Nothing
      readPlayers "[" `shouldBe` Nothing
      let ss =
            [ "{\"players\":[[[2,3],[4,4]],[[2,5],[4,4]]]," -- duplicates
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]]]," -- missing
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[]," -- missing
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[0,4]]]," -- out of range
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,6]]]," -- out of range
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,5]]],"
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1]]," -- missing
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,5]]],"
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0]]," -- missing
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,5]]],"
                ++ "\"spaces\":[[-1,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]," -- out of range
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,5]]],"
                ++ "\"spaces\":[[5,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]," -- out of range
                ++ "\"turn\":18}",
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,5]]],"
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":0}", -- invalid turn
              "{\"players\":[[[2,3],[4,4]],[[2,5],[1,5]]],"
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":-1}" -- invalid turn
            ]

      readBoard (ss !! 0) `shouldBe` Nothing
      readBoard (ss !! 1) `shouldBe` Nothing
      readBoard (ss !! 2) `shouldBe` Nothing
      readBoard (ss !! 3) `shouldBe` Nothing
      readBoard (ss !! 4) `shouldBe` Nothing
      readBoard (ss !! 5) `shouldBe` Nothing
      readBoard (ss !! 6) `shouldBe` Nothing
      readBoard (ss !! 7) `shouldBe` Nothing
      readBoard (ss !! 8) `shouldBe` Nothing
      readBoard (ss !! 9) `shouldBe` Nothing
      readBoard (ss !! 10) `shouldBe` Nothing

    it "succeeds with valid input" $ do
      let s =
            "{\"players\":[[[2,3],[4,4]],[[2,5],[3,5]]],"
              ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
              ++ "\"turn\":18}"
      readBoard s
        `shouldBe` Just
          Board
            { players = [((2, 3), (4, 4)), ((2, 5), (3, 5))],
              spaces = [[0, 0, 0, 0, 2], [1, 1, 2, 0, 0], [1, 0, 0, 3, 0], [0, 0, 3, 0, 0], [0, 0, 0, 1, 4]],
              turn = 18
            }

  describe "Board#readPlayers()" $ do
    it "fails with invalid input" $ do
      readPlayers "" `shouldBe` Nothing
      readPlayers "[" `shouldBe` Nothing
      readPlayers "[[[1,2],[True,3]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[0,3]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1,6]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1,2]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1,3,2]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1,3],[2,3]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1,3]],[[1,4],[1,2]]]" `shouldBe` Nothing
      readPlayers "[[[1,2],[1,3]],[[2,1],[4,3]],[[2,2],[3,3]]]" `shouldBe` Nothing

    it "succeeds with valid input" $ do
      readPlayers "[]" `shouldBe` Just []
      readPlayers "[[[1,2],[3,4]]]" `shouldBe` Just [((1, 2), (3, 4))]
      readPlayers "[[[1,2],[3,4]],[[2,1],[4,3]]]" `shouldBe` Just [((1, 2), (3, 4)), ((2, 1), (4, 3))]
      readPlayers "  [ [  [ 1 , 2],[3 , 4 ] \n],\n[  [2,1 ],[ 4, 3 ] ] ]  " `shouldBe` Just [((1, 2), (3, 4)), ((2, 1), (4, 3))]
