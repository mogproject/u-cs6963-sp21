module Game.GameStateSpec (spec) where

import Data.Bits (Bits ((.|.)))
import Data.Board (readBoard)
import qualified Data.Board as B
import Data.Card
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Game.BitBoard
import Game.GameState
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

-- rc to index
ri :: Int -> Int -> Int
ri r c = posToIndex (r, c)

getLegalMoveTo' :: B.Board -> [[Int]]
getLegalMoveTo' b =
  let GameState
        { cards = cs,
          players = pl,
          levelMap = lm,
          moveAdjacency = adj
        } = fromBoard b

      f wk =
        let mf = pl !! 0 !! wk
            opponentWorkers = sum . map singletonBB $ pl !! 1
            friend = singletonBB $ pl !! 0 !! (1 - wk)
            otherWorkers = opponentWorkers .|. friend
            allWorkers = otherWorkers .|. singletonBB (pl !! 0 !! wk)
            emptySpace = globalMask `andNotBB` ((lm ! 4) .|. otherWorkers)
         in getLegalMoveTo (cs !! 0) mf lm friend allWorkers emptySpace adj
   in [f wk | wk <- [0, 1]]

-- b: board after a move
getLegalBuildAt' :: B.Board -> Int -> Int -> [[(Index, Int)]]
getLegalBuildAt' b wk moveFrom =
  let GameState
        { cards = cs,
          players = pl,
          levels = lv,
          levelMap = lm
        } = fromBoard b

      opponentWorkers = sum . map singletonBB $ pl !! 1
      friendWorker = singletonBB $ pl !! 0 !! (1 - wk)
      otherWorkers = opponentWorkers .|. friendWorker
      emptySpace = globalMask `andNotBB` ((lm ! 4) .|. otherWorkers)
   in getLegalBuildAt (cs !! 0) lv emptySpace moveFrom (pl !! 0 !! wk)

spec :: Spec
spec = do
  describe "GameState#getLegalMoves()" $ do
    it "works" $ do
      let s1 = "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,2],[3,4]]},{\"card\":\"Apollo\",\"tokens\":[[1,3],[4,2]]}],\"spaces\":[[2,4,1,4,4],[4,2,4,2,2],[2,4,2,2,2],[2,2,4,4,4],[4,4,4,1,0]],\"turn\":70}"
      let st = fromBoard $ fromMaybe undefined (readBoard s1)
      (length . legalMoves) st `shouldBe` 21

  describe "GameState#getLegalMoveTo()" $ do
    it "works with Artemis" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((3, 3), (4, 5))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 4, 3, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }
      getLegalMoveTo' b1
        `shouldBe` [ [ri 1 2, ri 1 3, ri 2 1, ri 2 2, ri 2 4, ri 3 1],
                     [ri 2 4, ri 2 5, ri 3 5, ri 5 4, ri 5 5]
                   ]

    it "works with Minotaur" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Minotaur, B.tokens = Just ((5, 1), (4, 2))},
                    B.Player {B.card = Atlas, B.tokens = Just ((5, 2), (3, 3))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 0],
                    [0, 0, 0, 1, 0],
                    [2, 4, 2, 0, 0],
                    [3, 1, 4, 0, 0],
                    [0, 0, 3, 0, 0]
                  ],
                B.turn = 0
              }
      getLegalMoveTo' b1
        `shouldBe` [ [ri 5 2],
                     [ri 3 1, ri 3 3]
                   ]
      let b2 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Minotaur, B.tokens = Just ((3, 2), (2, 3))},
                    B.Player {B.card = Atlas, B.tokens = Just ((3, 3), (3, 4))}
                  ),
                B.spaces =
                  [ [0, 4, 4, 4, 0],
                    [4, 2, 2, 4, 0],
                    [4, 0, 1, 3, 0],
                    [4, 4, 2, 0, 4],
                    [0, 0, 0, 0, 0]
                  ],
                B.turn = 0
              }
      getLegalMoveTo' b2
        `shouldBe` [ [],
                     [ri 2 2, ri 3 3]
                   ]

    it "works with Apollo" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Apollo, B.tokens = Just ((5, 1), (4, 2))},
                    B.Player {B.card = Pan, B.tokens = Just ((5, 2), (3, 3))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 0],
                    [0, 0, 0, 1, 0],
                    [2, 4, 2, 0, 0],
                    [3, 1, 4, 0, 0],
                    [0, 0, 3, 0, 0]
                  ],
                B.turn = 0
              }
      getLegalMoveTo' b1
        `shouldBe` [ [ri 5 2],
                     [ri 3 1, ri 3 3, ri 5 2]
                   ]

    it "works with other cards" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Demeter, B.tokens = Just ((5, 1), (4, 2))},
                    B.Player {B.card = Hephastus, B.tokens = Just ((5, 2), (3, 3))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 0],
                    [0, 0, 0, 1, 0],
                    [2, 4, 2, 0, 0],
                    [3, 1, 4, 0, 0],
                    [0, 0, 3, 0, 0]
                  ],
                B.turn = 0
              }
      getLegalMoveTo' b1
        `shouldBe` [ [],
                     [ri 3 1]
                   ]

  describe "GameState#getLegalBuildAt()" $ do
    it "works with Atlas" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Atlas, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 4, 2, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b1 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 3, 4)],
                     [(ri 1 4, 1)],
                     [(ri 1 4, 4)],
                     [(ri 1 5, 3)],
                     [(ri 1 5, 4)],
                     [(ri 3 3, 3)],
                     [(ri 3 3, 4)],
                     [(ri 3 5, 4)]
                   ]
    it "works with Demeter" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Demeter, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 4, 2, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b1 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)],
                     [(ri 1 3, 2), (ri 1 4, 1)],
                     [(ri 1 3, 2), (ri 1 5, 3)],
                     [(ri 1 3, 2), (ri 3 3, 3)],
                     [(ri 1 3, 2), (ri 3 5, 4)],
                     [(ri 1 4, 1), (ri 1 5, 3)],
                     [(ri 1 4, 1), (ri 3 3, 3)],
                     [(ri 1 4, 1), (ri 3 5, 4)],
                     [(ri 1 5, 3), (ri 3 3, 3)],
                     [(ri 1 5, 3), (ri 3 5, 4)],
                     [(ri 3 3, 3), (ri 3 5, 4)]
                   ]

    it "works with Hephastus" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Hephastus, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 4, 2, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b1 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 3, 3)],
                     [(ri 1 4, 1)],
                     [(ri 1 4, 2)],
                     [(ri 1 5, 3)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)]
                   ]

    it "works with Prometheus" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Prometheus, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Pan, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 2, 2, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b1 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 2 3, 3)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)],
                     [(ri 1 3, 2), (ri 2 2, 2)],
                     [(ri 1 3, 2), (ri 2 3, 3)],
                     [(ri 1 4, 1), (ri 2 2, 2)],
                     [(ri 1 4, 1), (ri 2 3, 3)],
                     [(ri 1 5, 3), (ri 2 2, 2)],
                     [(ri 1 5, 3), (ri 2 3, 3)],
                     [(ri 2 3, 3), (ri 2 2, 2)],
                     [(ri 2 3, 4)],
                     [(ri 3 3, 3), (ri 2 2, 2)],
                     [(ri 3 3, 3), (ri 2 3, 3)],
                     [(ri 3 5, 4), (ri 2 2, 2)],
                     [(ri 3 5, 4), (ri 2 3, 3)]
                   ]

      let b2 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Prometheus, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Atlas, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 2, 2, 0],
                    [2, 4, 1, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b2 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 2 3, 3)],
                     [(ri 3 3, 2)],
                     [(ri 3 5, 4)]
                   ]

      let b3 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Prometheus, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Pan, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 2, 1, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b3 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 2 3, 3)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)],
                     [(ri 1 3, 2), (ri 2 2, 2)],
                     [(ri 1 3, 2), (ri 2 3, 3)],
                     [(ri 1 3, 2), (ri 2 4, 2)],
                     [(ri 1 4, 1), (ri 2 2, 2)],
                     [(ri 1 4, 1), (ri 2 3, 3)],
                     [(ri 1 4, 1), (ri 2 4, 2)],
                     [(ri 1 5, 3), (ri 2 2, 2)],
                     [(ri 1 5, 3), (ri 2 3, 3)],
                     [(ri 1 5, 3), (ri 2 4, 2)],
                     [(ri 2 3, 3), (ri 2 2, 2)],
                     [(ri 2 3, 4)],
                     [(ri 2 3, 3), (ri 2 4, 2)],
                     [(ri 3 3, 3), (ri 2 2, 2)],
                     [(ri 3 3, 3), (ri 2 3, 3)],
                     [(ri 3 3, 3), (ri 2 4, 2)],
                     [(ri 3 5, 4), (ri 2 2, 2)],
                     [(ri 3 5, 4), (ri 2 3, 3)],
                     [(ri 3 5, 4), (ri 2 4, 2)]
                   ]

      let b4 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Prometheus, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Pan, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 3, 2, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b4 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 2 3, 4)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)],
                     [(ri 1 3, 2), (ri 2 2, 2)],
                     [(ri 1 3, 2), (ri 2 3, 4)],
                     [(ri 1 4, 1), (ri 2 2, 2)],
                     [(ri 1 4, 1), (ri 2 3, 4)],
                     [(ri 1 5, 3), (ri 2 2, 2)],
                     [(ri 1 5, 3), (ri 2 3, 4)],
                     [(ri 2 3, 4), (ri 2 2, 2)],
                     [(ri 3 3, 3), (ri 2 2, 2)],
                     [(ri 3 3, 3), (ri 2 3, 4)],
                     [(ri 3 5, 4), (ri 2 2, 2)],
                     [(ri 3 5, 4), (ri 2 3, 4)]
                   ]

      let b5 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Prometheus, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Pan, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 2, 0, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b5 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 2 3, 3)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)],
                     [(ri 1 3, 2), (ri 2 2, 2)],
                     [(ri 1 3, 2), (ri 2 3, 3)],
                     [(ri 1 3, 2), (ri 2 4, 1)],
                     [(ri 1 4, 1), (ri 2 2, 2)],
                     [(ri 1 4, 1), (ri 2 3, 3)],
                     [(ri 1 4, 1), (ri 2 4, 1)],
                     [(ri 1 5, 3), (ri 2 2, 2)],
                     [(ri 1 5, 3), (ri 2 3, 3)],
                     [(ri 1 5, 3), (ri 2 4, 1)],
                     [(ri 2 3, 3), (ri 2 2, 2)],
                     [(ri 2 3, 4)],
                     [(ri 2 3, 3), (ri 2 4, 1)],
                     [(ri 3 3, 3), (ri 2 2, 2)],
                     [(ri 3 3, 3), (ri 2 3, 3)],
                     [(ri 3 3, 3), (ri 2 4, 1)],
                     [(ri 3 5, 4), (ri 2 2, 2)],
                     [(ri 3 5, 4), (ri 2 3, 3)],
                     [(ri 3 5, 4), (ri 2 4, 1)]
                   ]

    it "works with other cards" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Pan, B.tokens = Just ((2, 4), (2, 5))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((3, 4), (4, 4))}
                  ),
                B.spaces =
                  [ [3, 0, 1, 0, 2],
                    [0, 1, 4, 2, 0],
                    [2, 4, 2, 2, 3],
                    [0, 4, 4, 3, 3],
                    [0, 3, 3, 1, 1]
                  ],
                B.turn = 0
              }

      getLegalBuildAt' b1 0 (ri 3 3)
        `shouldBe` [ [(ri 1 3, 2)],
                     [(ri 1 4, 1)],
                     [(ri 1 5, 3)],
                     [(ri 3 3, 3)],
                     [(ri 3 5, 4)]
                   ]

  describe "GameState#makeMove()" $ do
    it "has no side effects" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Minotaur, B.tokens = Just ((3, 1), (5, 5))},
                    B.Player {B.card = Demeter, B.tokens = Just ((3, 5), (3, 2))}
                  ),
                B.spaces =
                  [ [3, 0, 2, 1, 2],
                    [2, 4, 0, 0, 1],
                    [1, 0, 1, 2, 0],
                    [4, 2, 4, 3, 1],
                    [4, 0, 0, 4, 0]
                  ],
                B.turn = 41
              }
      let s1 = fromBoard b1
      let moves = legalMoves s1
      let nextStates = [makeMove s1 m | m <- moves]

      (fromBoard . toBoard) (nextStates !! 10) `shouldBe` (nextStates !! 10)

      foldl
        (\_ x -> (fromBoard . toBoard) x `shouldBe` x)
        (True `shouldBe` True)
        nextStates

    modifyMaxSuccess (const 10000) $
      prop "has no side effects" $ \b ->
        let state = fromBoard b
            moves = legalMoves state
            nextStates = [makeMove state m | m <- moves]
         in do
              -- print state
              -- print $ head moves
              --  (fromBoard . toBoard) (head nextStates) `shouldBe` (head nextStates)
              foldl
                (\_ x -> (fromBoard . toBoard) x `shouldBe` x)
                (True `shouldBe` True)
                nextStates
