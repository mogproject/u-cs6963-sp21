module Search.SearchSpec (spec) where

import Data.Board (readBoard)
import qualified Data.Board as B
import Data.Card
import Data.Map (Map)
import qualified Data.Map
import Data.Maybe (fromMaybe)
import Game.BitBoard
import Game.Evaluation
import Game.GameMove
import Game.GameState (GameState (legalMoves), fromBoard, makeMove)
import Search.Search
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

printAllMoves :: GameState -> IO ()
printAllMoves st = putStr $ unlines [showMove m ++ ": " ++ show (evaluate (makeMove st m)) | m <- legalMoves st]

spec :: Spec
spec = do
  describe "Search#findMoveAlphaBeta()" $ do
    modifyMaxSuccess (const 20) $
      prop "result must be feasible" $ \b ->
        let s0 = fromBoard b
            (score, history) = findMoveAlphaBeta s0 2
            hs = reverse history
            moves = map snd hs
            states = scanl (\s (Just m) -> makeMove s m) s0 moves
         in do
              score >= (- scoreWin) `shouldBe` True
              score <= scoreWin `shouldBe` True

              tail states `shouldBe` map fst hs

  describe "Search#findMove()" $ do
    it "does not crash" $ do
      let s61 = "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,2],[3,4]]},{\"card\":\"Apollo\",\"tokens\":[[1,3],[4,2]]}],\"spaces\":[[2,4,1,4,4],[4,2,4,2,2],[2,4,2,2,2],[2,2,4,4,4],[4,4,4,1,0]],\"turn\":70}"
      let b61 = fromBoard $ fromMaybe undefined (readBoard s61)

      findMove 4 (mkStdGen 0) (Just 3) b61 > 0 `shouldBe` True

    it "finds a defensive move" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Minotaur, B.tokens = Just ((2, 5), (4, 5))},
                    B.Player {B.card = Artemis, B.tokens = Just ((5, 3), (3, 4))}
                  ),
                B.spaces =
                  [ [0, 0, 0, 0, 0],
                    [0, 0, 4, 4, 0],
                    [0, 3, 0, 0, 0],
                    [0, 0, 2, 4, 0],
                    [0, 0, 3, 0, 0]
                  ],
                B.turn = 20
              }
      let s1 = fromBoard b1

      -- putStr $ unlines [(showMove m) ++ ": " ++ show (evaluate (makeMove s1 m)) | m <- legalMoves s1]
      let (_, history) = findMoveAlphaBeta s1 3
      let hs = reverse history
      let moves = map snd hs
      let states = scanl (\s (Just m) -> makeMove s m) s1 moves
      tail states `shouldBe` map fst hs

      -- print $ map (maybe undefined showMove . snd) history

      let mv = findMove 4 (mkStdGen 0) (Just 3) s1
      -- print "chosen"
      -- putStr $ showMove mv

      -- printAllMoves s1

      getMoveFrom mv `shouldBe` posToIndex (4, 5)
      getMoveTo mv `shouldBe` posToIndex (5, 4)
      getBuildAt mv `shouldBe` [(posToIndex (4, 3), 2, 3)]

    it "does not make a losing move" $ do
      -- Player 1 (Demeter) is destined to lose
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Demeter, B.tokens = Just ((1, 5), (4, 4))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((2, 5), (4, 3))}
                  ),
                B.spaces =
                  [ [0, 0, 0, 4, 2],
                    [0, 1, 4, 1, 2],
                    [0, 0, 2, 3, 4],
                    [0, 0, 0, 0, 4],
                    [0, 0, 0, 2, 2]
                  ],
                B.turn = 17
              }
      let s1 = fromBoard b1

      -- ==== DEBUG START ====
      -- let (score, history) = findMoveAlphaBeta s1 3
      -- print score
      -- print $ map (maybe undefined showMove . snd) history
      -- printAllMoves s1

      -- ==== DEBUG END ====

      let mv = snd $ searchAlphaBetaNaive s1 2

      getMoveFrom mv `shouldBe` posToIndex (1, 5)
      getMoveTo mv `shouldBe` posToIndex (2, 4)
      let builds = getBuildAt mv
      builds `shouldContain` [(posToIndex (3, 4), 3, 4)]
      builds `shouldNotContain` [(posToIndex (1, 5), 2, 3)] -- losing build
    it "should make a losing move if all moves are losing" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Demeter, B.tokens = Just ((2, 3), (2, 4))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 1), (3, 2))}
                  ),
                B.spaces =
                  [ [0, 0, 0, 1, 0],
                    [0, 0, 0, 2, 0],
                    [0, 2, 1, 0, 1],
                    [3, 0, 1, 0, 0],
                    [0, 0, 0, 0, 0]
                  ],
                B.turn = 7
              }
      let s1 = fromBoard b1
      printAllMoves s1
      let mv = snd $ searchAlphaBetaNaive s1 2
      getLose mv `shouldBe` True

  describe "Search#searchAlphaBeta()" $ do
    it "finds the best move" $ do
      let nodes =
            Data.Map.fromList
              [ (0, [1, 2, 3]),
                (1, [4, 5]),
                (2, [6, 7, 8]),
                (3, []),
                (4, [9, 10]),
                (5, [11]),
                (6, []),
                (7, [12]),
                (8, [13, 14]),
                (9, []),
                (10, []),
                (11, []),
                (12, [15]),
                (13, []),
                (14, []),
                (15, [16]),
                (16, [])
              ] ::
              Map Int [Int]
      let scores =
            Data.Map.fromList
              [ (0, 0),
                (1, 0),
                (2, 10),
                (3, 4),
                (4, 4),
                (5, 5),
                (6, 30),
                (7, 7),
                (8, 8),
                (9, 20),
                (10, 5),
                (11, 30),
                (12, 10),
                (13, 5),
                (14, 25),
                (15, 15),
                (16, 50)
              ] ::
              Map Int Int

      let f = searchAlphaBeta (nodes Data.Map.!) (scores Data.Map.!) 0 50 0

      f True 1 `shouldBe` (10, [2])
      f True 2 `shouldBe` (7, [7, 2])
      f True 3 `shouldBe` (20, [9, 4, 1])
      f True 4 `shouldBe` (20, [9, 4, 1])
      f True 5 `shouldBe` (25, [14, 8, 2])
      f True 6 `shouldBe` (25, [14, 8, 2])

      f False 1 `shouldBe` (0, [1])
      f False 2 `shouldBe` (4, [3])
      f False 3 `shouldBe` (4, [3])
      f False 4 `shouldBe` (4, [3])
      f False 5 `shouldBe` (4, [3])
      f False 6 `shouldBe` (4, [3])

    it "finds the best move" $ do
      let nodes =
            Data.Map.fromList
              [ (0, [1, 2, 3, 4, 5, 6]),
                (1, [7, 8]),
                (2, []),
                (3, []),
                (4, []),
                (5, []),
                (6, []),
                (7, [9, 10]),
                (8, [11, 12]),
                (9, []),
                (10, []),
                (11, []),
                (12, [])
              ] ::
              Map Int [Int]
      let scores =
            Data.Map.fromList
              [ (0, 0),
                (1, -131224),
                (2, -1000000000),
                (3, -1000000000),
                (4, -1000000000),
                (5, -1000000000),
                (6, -1000000000),
                (7, -12345),
                (8, -12345),
                (9, -12346),
                (10, -12347),
                (11, -12348),
                (12, -12349)
              ] ::
              Map Int Int

      let f = searchAlphaBeta (nodes Data.Map.!) (scores Data.Map.!) (-1000000000) 1000000000 0

      f True 3 `shouldBe` (-12348, [11, 8, 1])

  describe "Search#findMoveWithTimeout()" $ do
    it "finds a winning move" $ do
      -- {"players":[{"card":"Artemis","tokens":[[1,4],[5,5]]},{"card":"Pan","tokens":[[2,4],[4,4]]}],"spaces":[[0,0,1,2,0],[0,0,0,0,4],[0,0,0,1,4],[0,2,2,0,4],[0,2,0,2,2]],"turn":26}

      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((1, 4), (5, 5))},
                    B.Player {B.card = Pan, B.tokens = Just ((2, 4), (4, 4))}
                  ),
                B.spaces = [[0, 0, 1, 2, 0], [0, 0, 0, 0, 4], [0, 0, 0, 1, 4], [0, 2, 2, 0, 4], [0, 2, 0, 2, 2]],
                B.turn = 26
              }
      let s1 = fromBoard b1
      -- printAllMoves s1

      mv <- findMoveWithTimeout 1000000 Nothing s1

      getMoveFrom mv `shouldBe` posToIndex (5, 5)
      getMoveTo mv `shouldBe` posToIndex (5, 4)
      getBuildAt mv `shouldBe` [(posToIndex (5, 5), 2, 3)]

    it "finds a defensive move" $ do
      -- {"players":[{"card":"Pan","tokens":[[2,4],[3,4]]},{"card":"Artemis","tokens":[[3,3],[5,5]]}],"spaces":[[1,0,1,2,0],[0,0,1,0,4],[0,0,0,1,4],[0,2,3,0,4],[0,2,0,2,2]],"turn":29}

      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Pan, B.tokens = Just ((2, 4), (3, 4))},
                    B.Player {B.card = Artemis, B.tokens = Just ((3, 3), (5, 5))}
                  ),
                B.spaces =
                  [ [1, 0, 1, 2, 0],
                    [0, 0, 1, 0, 4],
                    [0, 0, 0, 1, 4],
                    [0, 2, 3, 0, 4],
                    [0, 2, 0, 2, 2]
                  ],
                B.turn = 29
              }
      let s1 = fromBoard b1
      -- printAllMoves s1

      mv <- findMoveWithTimeout 1000000 Nothing s1

      getBuildAt mv `shouldBe` [(posToIndex (4, 3), 3, 4)]

    it "finds a defensive move" $ do
      -- {"players":[{"card":"Artemis","tokens":[[3,5],[4,1]]},{"card":"Prometheus","tokens":[[4,3],[4,4]]}],"spaces":[[0,3,0,0,0],[3,0,0,0,0],[0,2,0,4,1],[0,4,2,2,0],[1,1,4,0,0]],"turn":22}

      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((3, 5), (4, 1))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((4, 3), (4, 4))}
                  ),
                B.spaces = [[0, 3, 0, 0, 0], [3, 0, 0, 0, 0], [0, 2, 0, 4, 1], [0, 4, 2, 2, 0], [1, 1, 4, 0, 0]],
                B.turn = 29
              }
      let s1 = fromBoard b1
      -- printAllMoves s1

      mv2a <- findMoveWithTimeout 5000000 (Just 3) s1
      getBuildAt mv2a `shouldBe` [(posToIndex (2, 1), 3, 4)]

      mv2b <- findMoveWithTimeout 5000000 (Just 4) s1 -- acknowledges the loss
      getBuildAt mv2b `shouldBe` [(posToIndex (2, 1), 3, 4)]

      mv2c <- findMoveWithTimeout 5000000 Nothing s1
      getBuildAt mv2c `shouldBe` [(posToIndex (2, 1), 3, 4)]

    it "finds a defensive move" $ do
      -- {"players":[{"card":"Artemis","tokens":[[3,5],[5,2]]},{"card":"Prometheus","tokens":[[3,2],[4,4]]}],"spaces":[[0,3,0,0,0],[3,0,0,0,0],[0,1,0,4,1],[0,4,2,2,0],[1,0,4,0,0]],"turn":20}

      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((3, 5), (5, 2))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 2), (4, 4))}
                  ),
                B.spaces = [[0, 3, 0, 0, 0], [3, 0, 0, 0, 0], [0, 1, 0, 4, 1], [0, 4, 2, 2, 0], [1, 0, 4, 0, 0]],
                B.turn = 20
              }
      let s1 = fromBoard b1

      -- let s2 = makeMove s1 $ legalMoves s1 !! 6
      -- printAllMoves s2

      mv2c <- findMoveWithTimeout 5000000 Nothing s1
      -- print (showMove mv2c)    -- Worker[2]@(5,2) -> (3,1): ((2,1), lv=4)
      getBuildAt mv2c `shouldBe` [(posToIndex (2, 1), 3, 4)]

    it "finds a defensive move" $ do
      -- {"players":[{"card":"Artemis","tokens":[[4,3],[4,5]]},{"card":"Prometheus","tokens":[[3,3],[4,4]]}],"spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,1,2,0,0],[1,1,0,0,0],[1,1,0,2,0]],"turn":7}

      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((4, 3), (4, 5))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 3), (4, 4))}
                  ),
                B.spaces = [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 1, 2, 0, 0], [1, 1, 0, 0, 0], [1, 1, 0, 2, 0]],
                B.turn = 7
              }
      let s1 = fromBoard b1

      -- let s2 = makeMove s1 $ legalMoves s1 !! 6
      -- printAllMoves s2

      -- FIXME: decrease time limit
      mv <- findMoveWithTimeout 5000000 Nothing s1
      any (\p -> getMoveTo mv == p) [posToIndex (3, 2), posToIndex (4, 2)] `shouldBe` True
