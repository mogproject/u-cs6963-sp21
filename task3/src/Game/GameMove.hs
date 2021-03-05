module Game.GameMove
  ( GameMove,
    showMove,
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
    setBlocking,
    setStepping,
  )
where

import Data.Bits (complement, shift, (.&.), (.|.))
import Data.Int (Int64)
import Game.BitBoard (indexToPos)

type GameMove = Int64

--------------------------------------------------------------------------------
-- Bit representation
--------------------------------------------------------------------------------
-- smaller number => earlier candidate during search
--
-- 63    56 55    48 47    40 39    32 31    24 23    16 15     8 7      0
-- =======================================================================|
-- 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000|
--  x                                                                     | win      | 0: winning move; 1: otherwise
--   x                                                                    | lose     | 0: otherwise; 1: losing move
--       x                                                                | step bld | 0: build that enables my worker to move up; 1: otherwise
--        x                                                               | block bld| 0: build that disables one of the opponent's moving up; 1: otherwise
--          xxx                                                           | mv to lv | 7 - (move to level)
--                     xxx                                                | bld lv(2)| 7 - (level after build(2))
--                        xxx                                             | bld bl(2)| level before build(2)
--                            xxx                                         | bld lv(1)| 7 - (level after build(1))
--                               xxx                                      | bld bl(1)| level bfter build(1)
--                                  x                                     | dbl mv   | 0: double move; 1: otherwise
--                                   x                                    | mv op    | 0: opponent worker is pushed or swapped; 1: otherwise
--                                     xxxxxx                             | op mv to | 8-40: opponnet worker move to
--                                           x                            | op wid   | opponent worker id to be pushed or swapped; 0: worker 1; 1: worker 2
--                                            x xxxxx                     | bld (2)  | 8-40: index second build at
--                                                   xxx xxx              | bld (1)  | 8-40: index first build at
--                                                          xxxxx x       | mv to    | 8-40: index move to
--                                                                 xxxxxx | mv from  | 8-40: index move from
--                                                                       x| wid      | 0: worker 1; 1: worker 2

showMove :: GameMove -> String
showMove x =
  let workerId = (show . (+ 1) . getWorkerId) x
      moveFrom = (show . indexToPos . getMoveFrom) x
      moveTo = (show . indexToPos . getMoveTo) x
      builds = unwords ["(" ++ (show . indexToPos) buildAt ++ ", lv=" ++ show lv ++ ")" | (buildAt, _, lv) <- getBuildAt x]
   in concat
        [ "Worker[",
          workerId,
          "]@",
          moveFrom,
          " -> ",
          moveTo,
          ": ",
          builds
        ]

createGameMove :: GameMove
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
setMoveFrom = setValue 1 6

getMoveFrom :: Int64 -> Int
getMoveFrom = getValue 1 6

setMoveTo :: Int -> Int64 -> Int64
setMoveTo = setValue 7 6

getMoveTo :: Int64 -> Int
getMoveTo = getValue 7 6

setBuildAt :: [(Int, Int, Int)] -> Int64 -> Int64
setBuildAt [(bl, blb, bll)] = setValue 13 6 bl . setValue 34 3 blb . setValue 37 3 (7 - bll)
setBuildAt [(bl1, blb1, bll1), (bl2, blb2, bll2)] =
  (setValue 19 6 bl2 . setValue 40 3 blb2 . setValue 43 3 (7 - bll2)) . setBuildAt [(bl1, blb1, bll1)]
setBuildAt _ = undefined

getBuildAt :: Int64 -> [(Int, Int, Int)]
getBuildAt x =
  let blb1 = getValue 34 3 x
      bll1 = 7 - getValue 37 3 x
   in if bll1 == 0
        then []
        else
          let blb2 = getValue 40 3 x
              bll2 = 7 - getValue 43 3 x
           in if bll2 == 0
                then [(getValue 13 6 x, blb1, bll1)]
                else [(getValue 13 6 x, blb1, bll1), (getValue 19 6 x, blb2, bll2)]

setMoveToLevel :: Int -> Int64 -> Int64
setMoveToLevel lv = setValue 52 3 (7 - lv)

setOpponentMove :: Int -> Int -> Int64 -> Int64
setOpponentMove wid moveTo = setValue 32 1 0 . setValue 26 6 moveTo . setValue 25 1 wid

-- (wid, move to); wid=-1 if not applicable
getOpponentMove :: Int64 -> (Int, Int)
getOpponentMove x =
  if getValue 32 1 x == 0
    then (getValue 25 1 x, getValue 26 6 x)
    else (-1, 0)

setWin :: Int64 -> Int64
setWin = setValue 62 1 0

getWin :: Int64 -> Bool
getWin x = getValue 62 1 x == 0

setLose :: Int64 -> Int64
setLose = setValue 61 1 1

getLose :: Int64 -> Bool
getLose x = getValue 61 1 x == 1

setDoubleMove :: Int64 -> Int64
setDoubleMove = setValue 33 1 0

setDefence :: Int64 -> Int64
setDefence = setValue 45 1 0

setBlocking :: Int64 -> Int64
setBlocking = setValue 56 1 0

setStepping :: Int64 -> Int64
setStepping = setValue 57 1 0
