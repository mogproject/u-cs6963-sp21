module Game.GameStateSpec (spec) where

import Data.Bits (Bits (complement, shift, (.&.), (.|.)))
import Data.Board (readBoard)
import qualified Data.Board as B
import Data.Card
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Game.GameMove
import Game.GameState
import Test.Hspec

-- rc to index
ri :: Int -> Int -> Int
ri r c = (r - 1) * 5 + (c - 1)

getLegalMoveTo' :: B.Board -> [[Int]]
getLegalMoveTo' b =
  let GameState
        { cards = cs,
          players = pl,
          levelMap = lm,
          moveAdjacency = adjM
        } = fromBoard b

      f wk =
        let mf = pl !! 0 !! wk
            opponentWorkers = sum [1 `shift` x | x <- pl !! 1] :: Int
            friendWorker = 1 `shift` (pl !! 0 !! (1 - wk)) :: Int
            otherWorkers = opponentWorkers .|. friendWorker
            allWorkers = otherWorkers .|. (1 `shift` (pl !! 0 !! wk))
            emptySpace = ((1 `shift` 25) - 1) .&. complement ((lm ! 4) .|. otherWorkers) :: Bitmap
         in getLegalMoveTo (cs !! 0) mf lm friendWorker allWorkers emptySpace adjM
   in [f wk | wk <- [0, 1]]

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
