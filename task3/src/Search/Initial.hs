module Search.Initial
  ( findStartingPlayer1,
    findStartingPlayer2,
    player2Table,
  )
where

import Data.Board (Pos, Workers)
import Data.Card ( Card(Prometheus, Demeter, Hephastus) )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (StdGen, randomR)

allPositions :: Set Pos
allPositions = Set.fromList [(x, y) | x <- [1 .. 5], y <- [1 .. 5]]

retrieveElement :: StdGen -> Set a -> (a, Set a, StdGen)
retrieveElement rnd xs =
  let (n, gen) = randomR (0, Set.size xs - 1) rnd
   in (Set.elemAt n xs, Set.deleteAt n xs, gen)

findStartingPlayer1 :: Int -> StdGen -> Maybe (Card, Card) -> Workers
-- Strategy 0: random
findStartingPlayer1 0 gen _ =
  let (w1, ys, r1) = retrieveElement gen allPositions
      (w2, _, _) = retrieveElement r1 ys
   in (w1, w2)
-- Strategy 1: fixed position + randomized
findStartingPlayer1 1 gen (Just (_, c))
  | c `elem` [Demeter, Hephastus, Prometheus] =  -- vs. double builders
    let (n, _) = randomR (0, 3) gen
     in [((3, 3), (3, 2)), ((3, 3), (2, 3)), ((3, 3), (3, 4)), ((3, 3), (4, 3))] !! n
findStartingPlayer1 1 gen _ =
  let (n, _) = randomR (0, 3) gen
   in [((4, 4), (5, 5)), ((2, 2), (1, 1)), ((2, 4), (1, 5)), ((4, 2), (5, 1))] !! n
findStartingPlayer1 _ _ _ = undefined

player2TableBasis :: Map Workers Workers
player2TableBasis =
  Map.fromList
    [ (((1, 1), (1, 2)), ((2, 3), (3, 2))), -- 1
      (((1, 1), (1, 3)), ((2, 4), (3, 2))),
      (((1, 1), (1, 4)), ((2, 3), (3, 2))),
      (((1, 1), (1, 5)), ((2, 4), (3, 2))),
      (((1, 1), (2, 2)), ((2, 3), (3, 2))),
      (((1, 1), (2, 3)), ((3, 2), (3, 4))),
      (((1, 1), (2, 4)), ((3, 2), (3, 3))),
      (((1, 1), (2, 5)), ((3, 2), (3, 4))), -- 2
      (((1, 1), (3, 3)), ((3, 2), (3, 4))),
      (((1, 1), (3, 4)), ((3, 2), (3, 5))),
      (((1, 1), (3, 5)), ((2, 2), (3, 4))),
      (((1, 1), (4, 4)), ((2, 2), (3, 3))),
      (((1, 1), (4, 5)), ((2, 2), (3, 4))),
      (((1, 1), (5, 5)), ((2, 2), (3, 4))),
      (((1, 2), (1, 3)), ((3, 2), (3, 4))), -- 3
      (((1, 2), (1, 4)), ((2, 3), (3, 3))),
      (((1, 2), (2, 1)), ((2, 2), (3, 3))),
      (((1, 2), (2, 2)), ((2, 1), (3, 3))),
      (((1, 2), (2, 3)), ((3, 2), (3, 4))),
      (((1, 2), (2, 4)), ((2, 3), (3, 4))),
      (((1, 2), (2, 5)), ((2, 3), (3, 3))),
      (((1, 2), (3, 1)), ((2, 3), (3, 2))), -- 4
      (((1, 2), (3, 2)), ((3, 1), (3, 3))),
      (((1, 2), (3, 3)), ((3, 2), (3, 4))),
      (((1, 2), (3, 4)), ((2, 3), (4, 3))),
      (((1, 2), (3, 5)), ((2, 3), (4, 3))),
      (((1, 2), (4, 2)), ((2, 3), (4, 3))),
      (((1, 2), (4, 3)), ((3, 2), (3, 4))),
      (((1, 2), (4, 4)), ((2, 3), (3, 3))), -- 5
      (((1, 2), (4, 5)), ((2, 3), (3, 4))),
      (((1, 2), (5, 2)), ((2, 3), (4, 3))),
      (((1, 2), (5, 3)), ((2, 3), (4, 3))),
      (((1, 2), (5, 4)), ((2, 3), (4, 3))),
      (((1, 3), (2, 2)), ((3, 2), (3, 4))),
      (((1, 3), (2, 3)), ((3, 2), (3, 4))),
      (((1, 3), (3, 1)), ((2, 3), (3, 2))), -- 6
      (((1, 3), (3, 2)), ((2, 3), (4, 3))),
      (((1, 3), (3, 3)), ((3, 2), (3, 4))),
      (((1, 3), (4, 2)), ((2, 3), (3, 3))),
      (((1, 3), (4, 3)), ((2, 3), (5, 3))),
      (((1, 3), (5, 3)), ((2, 3), (4, 3))),
      (((2, 2), (2, 3)), ((3, 2), (3, 4))),
      (((2, 2), (2, 4)), ((3, 2), (3, 4))), -- 7
      (((2, 2), (3, 3)), ((2, 3), (4, 3))),
      (((2, 2), (3, 4)), ((2, 3), (4, 3))),
      (((2, 2), (4, 4)), ((2, 4), (3, 3))),
      (((2, 3), (3, 2)), ((1, 3), (4, 3))),
      (((2, 3), (3, 3)), ((3, 2), (3, 4))),
      (((2, 3), (4, 3)), ((3, 3), (3, 4)))
    ]

player2Table :: Map Workers Workers
player2Table = (update swap . update syms') player2TableBasis
  where
    update :: (Workers -> [Workers]) -> Map Workers Workers -> Map Workers Workers
    update f m =
      let xs = Map.toList m >>= (\(p1, p2) -> zip (f p1) (f p2))
       in Map.union m $ Map.fromList [x | x@(p1, _) <- xs, Map.notMember p1 m]
    syms (r, c) = [(r, 6 - c), (6 - r, c), (6 - r, 6 - c), (c, r), (c, 6 - r), (6 - c, r), (6 - c, 6 - r)]
    syms' (w1, w2) = zip (syms w1) (syms w2)
    swap (w1, w2) = [(w2, w1)]

findStartingPlayer2 :: Int -> StdGen -> Workers -> Maybe (Card, Card) -> Workers
-- Strategy 0: random
findStartingPlayer2 0 gen (u1, u2) _ =
  let xs = Set.delete u2 (Set.delete u1 allPositions)
      (w1, ys, r1) = retrieveElement gen xs
      (w2, _, _) = retrieveElement r1 ys
   in (w1, w2)
-- Strategy 1: predetermined position
findStartingPlayer2 1 gen ws c =
  Map.findWithDefault (findStartingPlayer2 0 gen ws c) ws player2Table
findStartingPlayer2 _ _ _ _ = undefined
