module Search.Initial
  ( findStartingPlayer1,
    findStartingPlayer2,
  )
where

import Data.Board (Pos, Workers)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (StdGen, mkStdGen, randomR)

allPositions :: Set Pos
allPositions = Set.fromList [(x, y) | x <- [1 .. 5], y <- [1 .. 5]]

retrieveElement :: StdGen -> Set a -> (a, Set a, StdGen)
retrieveElement rnd xs =
  let (n, gen) = randomR (0, Set.size xs - 1) rnd
   in (Set.elemAt n xs, Set.deleteAt n xs, gen)

findStartingPlayer1 :: Int -> Int -> Workers
findStartingPlayer1 strategy seed | strategy == 0 =
    let r0 = mkStdGen seed
        (w1, ys, r1) = retrieveElement r0 allPositions
        (w2, _, _) = retrieveElement r1 ys
    in (w1, w2)
findStartingPlayer1 strategy seed = ((1,1),(2,2))

findStartingPlayer2 :: Int -> Workers -> Workers
findStartingPlayer2 seed (u1, u2) =
  let r0 = mkStdGen seed
      xs = Set.delete u2 (Set.delete u1 allPositions)
      (w1, ys, r1) = retrieveElement r0 xs
      (w2, _, _) = retrieveElement r1 ys
   in (w1, w2)
