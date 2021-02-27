module Data.BoardSpec (spec) where

import Data.Board
  ( Board (Board),
    Player (Player),
    Players,
    card,
    choosePlayers,
    players,
    readBoard,
    readPlayers,
    spaces,
    tokens,
    turn,
    writeBoard,
    writePlayers,
  )
import Data.Card (Card (Apollo, Artemis, Atlas, Demeter, Hephastus, Minotaur, Pan, Prometheus))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Gen for property-based testing
--------------------------------------------------------------------------------

-- Wapper of Players
newtype Players' = Players' Players deriving (Show)

fromPlayers' :: Players' -> Players
fromPlayers' (Players' p) = p

instance Arbitrary Players' where
  arbitrary = do
    numPlayers <- chooseInt (0, 2)
    pl <- choosePlayers numPlayers
    return $ Players' pl

spec :: Spec
spec = do
  describe "Board#readPlayers()" $ do
    it "fails with invalid input" $ do
      readPlayers "" `shouldBe` Nothing
      readPlayers "[" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[True,3]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[0,3]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[1,6]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[1,2]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[1,3,2]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[1]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\"=[[1,2],[1,3],[2,3]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\",\"tokens\"=[[1,2],[1,3]]},{\"card\":\"Prometheus\",\"tokens\"=[[1,4],[1,2]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"Artemis\",\"tokens\"=[[1,2],[1,3]]},{\"card\":\"Prometheus\",\"tokens\"=[[2,1],[4,3]]},{\"card\":\"Atlas\",\"tokens\"=[[2,2],[3,3]]}]" `shouldBe` Nothing
      readPlayers "[{\"card\":[1,2]},{\"card\":\"Prometheus\"}]" `shouldBe` Nothing
      readPlayers "[{\"card\":\"artemis\"},{\"card\":\"Prometheus\"}]" `shouldBe` Nothing -- lower case
    it "succeeds with valid input" $ do
      readPlayers "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]"
        `shouldBe` Just (Player {card = Artemis, tokens = Nothing}, Player {card = Prometheus, tokens = Nothing})
      readPlayers "[{\"card\":\"Prometheus\"},{\"card\":\"Artemis\",\"tokens\":[[1,2],[3,4]]}]"
        `shouldBe` Just (Player {card = Prometheus, tokens = Nothing}, Player {card = Artemis, tokens = Just ((1, 2), (3, 4))})
      readPlayers "[{\"card\":\"Artemis\",\"tokens\":[[1,2],[3,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,1],[4,3]]}]"
        `shouldBe` Just (Player {card = Artemis, tokens = Just ((1, 2), (3, 4))}, Player {card = Prometheus, tokens = Just ((2, 1), (4, 3))})
      readPlayers "  [ {  \"card\":\"Artemis\",\"tokens\":[[1,  2 ] ,  [  3, 4 ] ]},{\"card\":\"Prometheus\",\"tokens\":[[2,1],[4,3]]}]  "
        `shouldBe` Just (Player {card = Artemis, tokens = Just ((1, 2), (3, 4))}, Player {card = Prometheus, tokens = Just ((2, 1), (4, 3))})

  describe "Board#writePlayers()" $ do
    it "returns JSON string" $ do
      writePlayers (Player {card = Artemis, tokens = Nothing}, Player {card = Prometheus, tokens = Nothing})
        `shouldBe` "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]"
      writePlayers (Player {card = Prometheus, tokens = Nothing}, Player {card = Artemis, tokens = Just ((1, 2), (3, 4))})
        `shouldBe` "[{\"card\":\"Prometheus\"},{\"card\":\"Artemis\",\"tokens\":[[1,2],[3,4]]}]"
      writePlayers (Player {card = Artemis, tokens = Just ((1, 2), (3, 4))}, Player {card = Prometheus, tokens = Just ((2, 1), (4, 3))})
        `shouldBe` "[{\"card\":\"Artemis\",\"tokens\":[[1,2],[3,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,1],[4,3]]}]"

  describe "Board#readPlayers()" $ do
    prop "is inverse to writePlayers()" $ \pp ->
      let p = fromPlayers' pp
       in (readPlayers . writePlayers) p `shouldBe` Just p

  describe "Board#readBoard()" $ do
    it "fails with invalid input" $ do
      readBoard "" `shouldBe` Nothing
      readPlayers "[" `shouldBe` Nothing
      let ss =
            [ "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[4,4]]}]," -- duplicates
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\",\"tokens\":[[2,3],[4,4]]}]," -- missing
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]," -- missing tokens
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[{\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[3,5]]}],"  -- missing card
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[0,4]]}]," -- out of range
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[1,6]]}]," -- out of range
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[1,5]]}],"
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1]]," -- missing
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[1,5]]}],"
                ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0]]," -- missing
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[1,5]]}],"
                ++ "\"spaces\":[[-1,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]," -- out of range
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[1,5]]}],"
                ++ "\"spaces\":[[5,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]," -- out of range
                ++ "\"turn\":18}",
              "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[1,5]]}],"
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
    let ss =
          [ "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[3,5]]}],"
              ++ "\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"
              ++ "\"turn\":18}",
            "{\"players\":[{\"card\":\"Minotaur\",\"tokens\":[[5,5],[4,4]]},{\"card\":\"Atlas\",\"tokens\":[[3,3],[2,2]]}],"
              ++ "\"spaces\":[[0,1,2,3,4],[0,1,2,3,4],[0,1,2,3,4],[0,1,2,3,4],[0,1,2,3,4]],"
              ++ "\"turn\":0}"
          ]
    readBoard (ss !! 0)
      `shouldBe` Just
        Board
          { players = (Player {card = Artemis, tokens = Just ((2, 3), (4, 4))}, Player {card = Prometheus, tokens = Just ((2, 5), (3, 5))}),
            spaces = [[0, 0, 0, 0, 2], [1, 1, 2, 0, 0], [1, 0, 0, 3, 0], [0, 0, 3, 0, 0], [0, 0, 0, 1, 4]],
            turn = 18
          }
    readBoard (ss !! 1)
      `shouldBe` Just
        Board
          { players = (Player {card = Minotaur, tokens = Just ((5, 5), (4, 4))}, Player {card = Atlas, tokens = Just ((3, 3), (2, 2))}),
            spaces = [[0, 1, 2, 3, 4], [0, 1, 2, 3, 4], [0, 1, 2, 3, 4], [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]],
            turn = 0
          }

  describe "Board#writeBoard()" $ do
    it "returns JSON string" $ do
      let b1 =
            Board
              { players = (Player {card = Demeter, tokens = Just ((2, 3), (4, 4))}, Player {card = Hephastus, tokens = Just ((2, 5), (3, 5))}),
                spaces = [[0, 0, 0, 0, 2], [1, 1, 2, 0, 0], [1, 0, 0, 3, 0], [0, 0, 3, 0, 0], [0, 0, 0, 1, 4]],
                turn = 18
              }
      let b2 =
            Board
              { players = (Player {card = Pan, tokens = Just ((5, 5), (4, 4))}, Player {card = Apollo, tokens = Just ((3, 3), (2, 2))}),
                spaces = [[0, 1, 2, 3, 4], [0, 1, 2, 3, 4], [0, 1, 2, 3, 4], [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]],
                turn = 0
              }
      writeBoard b1 `shouldBe` "{\"turn\":18,\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],\"players\":[{\"card\":\"Demeter\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Hephastus\",\"tokens\":[[2,5],[3,5]]}]}"
      writeBoard b2 `shouldBe` "{\"turn\":0,\"spaces\":[[0,1,2,3,4],[0,1,2,3,4],[0,1,2,3,4],[0,1,2,3,4],[0,1,2,3,4]],\"players\":[{\"card\":\"Pan\",\"tokens\":[[5,5],[4,4]]},{\"card\":\"Apollo\",\"tokens\":[[3,3],[2,2]]}]}"

  describe "Board#readBoard()" $ do
    prop "is inverse to writeBoard()" $ \b -> (readBoard . writeBoard) b `shouldBe` Just (b :: Board)
