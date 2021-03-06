module Game.GameStateSpec (spec) where

import Data.Bits (Bits ((.&.), (.|.)))
import Data.Board (readBoard)
import qualified Data.Board as B
import Data.Card
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as Map
import Data.Maybe (fromMaybe)
import Game.BitBoard
import Game.GameState
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

-- import Game.GameMove (showMove)

-- rc to index
ri :: Int -> Int -> Int
ri r c = posToIndex (r, c)

getLegalMoveTo'' :: B.Board -> [[Int]]
getLegalMoveTo'' b =
  let GameState
        { cards = cs,
          players = pl,
          playerMap = pm,
          levels = lv,
          levelMap = lm
        } = fromBoard b

      f wk = getLegalMoveTo (cs !! 0) (pl !! 0 !! wk) pm lv lm
   in [f wk | wk <- [0, 1]]

-- b: board after a move
getLegalBuildAt' :: B.Board -> Int -> Int -> [[(Index, Int)]]
getLegalBuildAt' b wk moveFrom =
  let GameState
        { cards = cs,
          players = pl,
          playerMap = pm,
          levels = lv,
          levelMap = lm
        } = fromBoard b
      emptySpace = ((lm ! 7) `andNotBB` (pm ! 6)) .|. singletonBB (pl !! 0 !! wk)
      result = getLegalBuildAt (cs !! 0) lv emptySpace moveFrom (pl !! 0 !! wk)
   in [[(x, z) | (x, _, z) <- r] | r <- result]

spec :: Spec
spec = do
  describe "GameState#getLegalMoves()" $ do
    it "removes losing moves" $ do
      let s1 = "{\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,2],[3,4]]},{\"card\":\"Apollo\",\"tokens\":[[1,3],[4,2]]}],\"spaces\":[[2,4,1,4,4],[4,2,4,2,2],[2,4,2,2,2],[2,2,4,4,4],[4,4,4,1,0]],\"turn\":70}"
      let st = fromBoard $ fromMaybe undefined (readBoard s1)
      (length . getLegalMoves' False) st `shouldBe` 21
      (length . getLegalMoves' True) st `shouldBe` 17

    it "works with Artemis" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((5, 3), (3, 4))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((2, 5), (5, 4))}
                  ),
                B.spaces =
                  [ [0, 0, 0, 0, 0],
                    [0, 0, 4, 4, 0],
                    [0, 3, 0, 0, 0],
                    [0, 0, 3, 4, 0],
                    [0, 0, 3, 0, 0]
                  ],
                B.turn = 18
              }
      let s1 = fromBoard b1
      (length . legalMoves) s1 `shouldBe` 67

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
      getLegalMoveTo'' b1
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
      getLegalMoveTo'' b1
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
      getLegalMoveTo'' b2
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
      getLegalMoveTo'' b1
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
      getLegalMoveTo'' b1
        `shouldBe` [ [],
                     [ri 3 1]
                   ]

  describe "GameState#getLegalBuildAt()" $ do
    it "works with Artemis" $ do
      let b1 =
            B.Board
              { B.players =
                  ( B.Player {B.card = Artemis, B.tokens = Just ((3, 2), (3, 4))},
                    B.Player {B.card = Prometheus, B.tokens = Just ((2, 5), (5, 4))}
                  ),
                B.spaces =
                  [ [0, 0, 0, 0, 0],
                    [0, 0, 4, 4, 0],
                    [0, 3, 0, 0, 0],
                    [0, 0, 3, 4, 0],
                    [0, 0, 3, 0, 0]
                  ],
                B.turn = 18
              }

      getLegalBuildAt' b1 0 (ri 5 3)
        `shouldBe` [ [(ri 2 1, 1)],
                     [(ri 2 2, 1)],
                     [(ri 3 1, 1)],
                     [(ri 3 3, 1)],
                     [(ri 4 1, 1)],
                     [(ri 4 2, 1)],
                     [(ri 4 3, 4)]
                   ]
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

  describe "GameState#hasWinningMove()" $ do
    it "works with Minotaur" $ do
      let pm1 = createPlayerMap [[ri 1 3, ri 3 4], [ri 2 3, ri 4 5]]
      let pm1' = createPlayerMap [[ri 2 3, ri 4 5], [ri 1 3, ri 3 4]]
      let lv =
            Map.fromList $
              (zip validIndices . concat)
                [ [3, 0, 3, 1, 2],
                  [2, 4, 2, 0, 1],
                  [1, 0, 1, 3, 0],
                  [4, 2, 4, 3, 1],
                  [4, 0, 0, 4, 0]
                ]
      let lm = createLevelMap lv
      hasWinningMove (Just Minotaur) 0 pm1' lm `shouldBe` False
      hasWinningMove (Just Minotaur) 1 pm1 lm `shouldBe` False

      let pm2 = createPlayerMap [[ri 1 3, ri 3 4], [ri 2 3, ri 5 5]]
      let pm2' = createPlayerMap [[ri 2 3, ri 5 5], [ri 1 3, ri 3 4]]
      hasWinningMove (Just Minotaur) 0 pm2' lm `shouldBe` True
      hasWinningMove (Just Minotaur) 1 pm2 lm `shouldBe` True

    it "works with Apollo" $ do
      let pm1 = createPlayerMap [[ri 1 4, ri 3 3], [ri 2 3, ri 4 5]]
      let pm1' = createPlayerMap [[ri 2 3, ri 4 5], [ri 1 4, ri 3 3]]
      let lv =
            Map.fromList $
              (zip validIndices . concat)
                [ [3, 0, 3, 1, 2],
                  [2, 4, 2, 0, 1],
                  [1, 0, 1, 3, 0],
                  [4, 2, 4, 3, 1],
                  [4, 0, 0, 4, 0]
                ]
      let lm = createLevelMap lv
      hasWinningMove (Just Apollo) 0 pm1' lm `shouldBe` True
      hasWinningMove (Just Apollo) 1 pm1 lm `shouldBe` True

    it "works with Artemis" $ do
      let pm1 = createPlayerMap [[ri 2 4, ri 3 4], [ri 3 3, ri 5 5]]
      let lv = Map.fromList $
              (zip validIndices . concat)
                  [ [1, 0, 1, 2, 0],
                    [0, 0, 1, 0, 4],
                    [0, 0, 0, 1, 4],
                    [0, 2, 3, 0, 4],
                    [0, 2, 0, 2, 2]
                  ]
      let lm = createLevelMap lv
      hasWinningMove (Just Artemis) 1 pm1 lm `shouldBe` True

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
