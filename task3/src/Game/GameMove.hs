module Game.GameMove
  ( GameMove,
    createGameMove,
    setWorkerId,
    getWorkerId,
    setMoveFrom,
    getMoveFrom,
    setMoveTo,
    getMoveTo,
    setWin,
    getWin,
    setLose,
    getLose,
    setMoveToLevel,
    setOpponentMove,
    getOpponentMove,
    setDoubleMove,
    setBuildAt,
    getBuildAt,
    setDefence,
  )
where

import Data.Bits (complement, shift, (.&.), (.|.))
import Data.Int (Int64)

type GameMove = Int64

--------------------------------------------------------------------------------
-- Bit representation
--------------------------------------------------------------------------------
-- smaller number -> earlier candidate during search
--
-- 63    56 55    48 47    40 39    32 31    24 23    16 15     8 7      0
-- =======================================================================|
-- 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000|
--                   x                                                    | win      | 0: winning move; 1: otherwise
--                    x                                                   | lose     | 0: otherwise; 1: losing move
--                     x                                                  | defence  | 0: disables at least one of the opponent's winning moves; 1: otherwise
--                      x                                                 | step bld | 0: build that enables my worker to move up; 1: otherwise
--                       x                                                | block bld| 0: build that disables one of the opponent's moving up; 1: otherwise
--                        xxx                                             | mv to lv | 7 - (move to level)
--                            xxx                                         | bld lv(2)| 7 - (level after build(2))
--                               xxx                                      | bld lv(1)| 7 - (level after build(1))
--                                  x                                     | dbl mv   | 0: double move; 1: otherwise
--                                   x                                    | mv op    | 0: opponent worker is pushed or swapped; 1: otherwise
--                                          xxx xx                        | op mv to | 0-24: opponnet worker move to
--                                                x                       | op wid   | opponent worker id to be pushed or swapped; 0: worker 1; 1: worker 2
--                                                 xxxxx                  | bld (2)  | 0-24: index second build at
--                                                       xxxxx            | bld (1)  | 0-24: index first build at
--                                                            xxx xx      | mv to    | 0-24: index move to
--                                                                  xxxxx | mv from  | 0-24: index move from
--                                                                       x| wid      | 0: worker 1; 1: worker 2

createGameMove :: Int64
createGameMove = setValue 46 1 0 $ (((1 :: Int64) `shift` 31) - 1) `shift` 32

getMask :: Int -> Int -> Int64
getMask offset len = (((1 :: Int64) `shift` len) - 1) `shift` offset

setValue :: Int -> Int -> Int -> Int64 -> Int64
setValue offset len i x = (fromIntegral i `shift` offset) .|. (fromIntegral x .&. complement (getMask offset len))

getValue :: Int -> Int -> Int64 -> Int
getValue offset len x = fromIntegral $ (x .&. getMask offset len) `shift` (- offset)

setWorkerId :: Int -> Int64 -> Int64
setWorkerId = setValue 0 1

getWorkerId :: Int64 -> Int
getWorkerId = getValue 0 1

setMoveFrom :: Int -> Int64 -> Int64
setMoveFrom = setValue 1 5

getMoveFrom :: Int64 -> Int
getMoveFrom = getValue 1 5

setMoveTo :: Int -> Int64 -> Int64
setMoveTo = setValue 6 5

getMoveTo :: Int64 -> Int
getMoveTo = getValue 6 5

setMoveToLevel :: Int -> Int64 -> Int64
setMoveToLevel lv = setValue 40 3 (7 - lv)

setOpponentMove :: Int -> Int -> Int64 -> Int64
setOpponentMove wid moveTo = setValue 32 1 0 . setValue 22 5 moveTo . setValue 21 1 wid

-- (wid, move to); wid=-1 if not applicable
getOpponentMove :: Int64 -> (Int, Int)
getOpponentMove x =
  if getValue 32 1 x == 0
    then (getValue 21 1 x, getValue 22 5 x)
    else (-1, 0)

setWin :: Int64 -> Int64
setWin = setValue 47 1 0

getWin :: Int64 -> Bool
getWin x = getValue 47 1 x == 0

setLose :: Int64 -> Int64
setLose = setValue 46 1 1

getLose :: Int64 -> Bool
getLose x = getValue 46 1 x == 1

setDoubleMove :: Int64 -> Int64
setDoubleMove = setValue 33 1 0

setDefence :: Int64 -> Int64
setDefence = setValue 45 1 0

setBuildAt :: [(Int, Int)] -> Int64 -> Int64
setBuildAt [(bl, bll)] = setValue 11 5 bl . setValue 34 3 (7 - bll)
setBuildAt [(bl1, bll1), (bl2, bll2)] = (setValue 16 5 bl2 . setValue 37 3 (7 - bll2)) . setBuildAt [(bl1, bll1)]
setBuildAt _ = undefined

getBuildAt :: Int64 -> [(Int, Int)]
getBuildAt x =
  let bll1 = 7 - getValue 34 3 x
   in if bll1 == 0
        then []
        else
          let bll2 = 7 - getValue 37 3 x
           in if bll2 == 0
                then [(getValue 11 5 x, bll1)]
                else [(getValue 11 5 x, bll1), (getValue 16 5 x, bll2)]
