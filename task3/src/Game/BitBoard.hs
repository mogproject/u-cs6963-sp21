module Game.BitBoard
  ( BitBoard,
    rcToIndex,
    globalMask,
    showBB,
    singletonBB,
    listToBB,
    bbToList,
    countBB,
    getNeighborhood,
    getClosedNeighborhood,
  ) 
where

import Data.Int (Int64)
import Data.Bits (shift, (.&.), (.|.), popCount, xor, countTrailingZeros)

-- Bitwise representation of the board
--
--          col    1   2   3   4   5
--          =============================
--             0   1   2   3   4   5   6
-- row           --------------------
--  1          7 | 8   9  10  11  12| 13
--  2         14 |15  16  17  18  19| 20
--  3         21 |22  23  24  25  26| 27
--  4         28 |29  30  31  32  33| 34
--  5         35 |36  37  38  39  40| 41
--               --------------------

type Index = Int
type BitBoard = Int64

rcToIndex :: Int -> Int -> Index
rcToIndex r c = 7 * r + c

elemBB :: Index -> BitBoard -> Bool
elemBB i bb = (bb `shift` (- i)) .&. 1 == 1

bbToList :: BitBoard -> [Index]
bbToList bb = tail . map fst $ takeWhile ((/= -1) . fst) $ iterate f (0, bb)
  where
    f (_, y) =
      if y == 0
        then (-1, 0)
        else
          let p = countTrailingZeros y
           in (p, y `xor` (1 `shift` p))

-- it is user's responsibility to keep bits clean
countBB :: BitBoard -> Int
countBB = popCount

singletonBB :: Int -> BitBoard
singletonBB i = 1 `shift` i

listToBB :: [Int] -> BitBoard
listToBB = sum . map singletonBB

-- mask of valid bits (should be 2147077824256)
globalMask :: BitBoard
globalMask = sum [singletonBB (rcToIndex r c) | r <- [1..5], c <- [1..5]]

getNeighborhood :: Int -> BitBoard
getNeighborhood i = x `shift` i `shift` s .&. globalMask
  where x = 115335 -- listToBB [0, 1, 2, 7, 9, 14, 15, 16]
        s = -8

getClosedNeighborhood :: BitBoard -> BitBoard
getClosedNeighborhood bb = let bb' = bb .|. (bb `shift` 1) .|. (bb `shift` (-1))
                               bb'' = bb' .|. (bb' `shift` 7) .|. (bb' `shift` (-7))
                            in bb'' .&. globalMask

showBB :: BitBoard -> String
showBB bb = unlines [concat [if rcToIndex r c `elemBB` bb then "*" else "-" | c <- [1..5]] | r <- [1..5]]
