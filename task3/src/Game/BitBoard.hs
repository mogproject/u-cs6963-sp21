module Game.BitBoard
  ( BitBoard,
    Index,
    posToIndex,
    indexToPos,
    elemBB,
    andNotBB,
    globalMask,
    showBB,
    singletonBB,
    listToBB,
    bbToList,
    countBB,
    getNeighborhood,
    getClosedNeighborhood,
    validIndices,
    isValidIndex,
    getPointSymmetricIndex,
  )
where

import Data.Bits (Bits (complement), countTrailingZeros, popCount, shift, xor, (.&.), (.|.))
import Data.Int (Int64)

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
--            42  43  44  45  46  47  48

type Index = Int

type BitBoard = Int64

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

posToIndex :: (Int, Int) -> Index
posToIndex (r, c) = 7 * r + c

indexToPos :: Index -> (Int, Int)
indexToPos i = (i `div` 7, i `mod` 7)

showBB :: BitBoard -> String
showBB bb = unlines [concat [if posToIndex (r, c) `elemBB` bb then "*" else "-" | c <- [1 .. 5]] | r <- [1 .. 5]]

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

singletonBB :: Int -> BitBoard
singletonBB i = 1 `shift` i

validIndices :: [Index]
validIndices = [posToIndex (r, c) | r <- [1 .. 5], c <- [1 .. 5]]

-- mask of valid bits (should be 2147077824256)
globalMask :: BitBoard
globalMask = sum . map singletonBB $ validIndices

bbToList :: BitBoard -> [Index]
bbToList bb = tail . map fst $ takeWhile ((/= -1) . fst) $ iterate f (0, bb)
  where
    f (_, y) =
      if y == 0
        then (-1, 0)
        else
          let p = countTrailingZeros y
           in (p, y `xor` (1 `shift` p))

listToBB :: [Int] -> BitBoard
listToBB = sum . map singletonBB

--------------------------------------------------------------------------------
-- Basic Operations
--------------------------------------------------------------------------------

elemBB :: Index -> BitBoard -> Bool
elemBB i bb = (bb `shift` (- i)) .&. 1 == 1

andNotBB :: BitBoard -> BitBoard -> BitBoard
andNotBB bb x = bb .&. complement x

isValidIndex :: Index -> Bool
isValidIndex i = elemBB i globalMask

getPointSymmetricIndex :: Index -> Index -> Index
getPointSymmetricIndex center i = center * 2 - i

-- it is user's responsibility to keep bits clean
countBB :: BitBoard -> Int
countBB = popCount

--------------------------------------------------------------------------------
-- Neighborhood
--------------------------------------------------------------------------------

getNeighborhood :: Int -> BitBoard
getNeighborhood i = x `shift` (i + s) .&. globalMask
  where
    x = 115335 -- listToBB [0, 1, 2, 7, 9, 14, 15, 16]
    s = -8

getClosedNeighborhood :: BitBoard -> BitBoard
getClosedNeighborhood bb =
  let x = bb .|. (bb `shift` 1) .|. (bb `shift` (-1))
      y = x .|. (x `shift` 7) .|. (x `shift` (-7))
   in y .&. globalMask
