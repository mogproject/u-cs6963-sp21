module Search.Initial
  ( findStartingPlayer1,
    findStartingPlayer2,
    player2Table,
  )
where

import Data.Board (Player(Player), Players, Pos, Workers, card, tokens)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (StdGen, mkStdGen, randomR)

allPositions :: Set Pos
allPositions = Set.fromList [(x, y) | x <- [1 .. 5], y <- [1 .. 5]]

retrieveElement :: StdGen -> Set a -> (a, Set a, StdGen)
retrieveElement rnd xs =
  let (n, gen) = randomR (0, Set.size xs - 1) rnd
   in (Set.elemAt n xs, Set.deleteAt n xs, gen)

findStartingPlayer1 :: Int -> Int -> Players -> Players
-- Strategy 0: random
findStartingPlayer1 strategy seed (Player {card = c}, p2@Player {card = _})
  | strategy == 0 =
    let r0 = mkStdGen seed
        (w1, ys, r1) = retrieveElement r0 allPositions
        (w2, _, _) = retrieveElement r1 ys
     in (p2, Player {card = c, tokens = Just (w1, w2)})
-- Strategy 1: fixed position
findStartingPlayer1 strategy _ (Player {card = c}, p2@Player {card = _})
  | strategy == 1 = (p2, Player {card = c, tokens = Just ((4, 4), (5, 5))})
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

getRandomPositions :: Int -> Workers -> Workers
getRandomPositions seed (u1, u2) =
  let r0 = mkStdGen seed
      xs = Set.delete u2 (Set.delete u1 allPositions)
      (w1, ys, r1) = retrieveElement r0 xs
      (w2, _, _) = retrieveElement r1 ys
   in (w1, w2)

findStartingPlayer2 :: Int -> Int -> Players -> Players
-- Strategy 0: random
findStartingPlayer2 strategy seed (Player {card = c}, p1@Player {card = _, tokens = Just us})
  | strategy == 0 = (p1, Player {card = c, tokens = Just (getRandomPositions seed us)})
-- Strategy 1: predetermined position
findStartingPlayer2 strategy seed (Player {card = c}, p1@Player {card = _, tokens = Just ws})
  | strategy == 1 =
    let ww = Map.findWithDefault (getRandomPositions seed ws) ws player2Table
     in (p1, Player {card = c, tokens = Just ww})
findStartingPlayer2 _ _ _ = undefined
