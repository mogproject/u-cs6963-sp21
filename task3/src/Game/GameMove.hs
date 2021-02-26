module Game.GameMove
  ( GameMove
  )
where

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
--          x                                                             | win      | 0: winning move; 1: otherwise
--           x                                                            | defence  | 0: disables all of the opponent's winning moves; 1: otherwise
--            x                                                           | step bld | 0: build that enables my worker to move up; 1: otherwise
--             x                                                          | block bld| 0: build that disables one of the opponent's moving up; 1: otherwise
--              xxx                                                       | mv to lv | 7 - (move to level)
--                 x xx                                                   | bld lv(2)| 7 - (level after build(2))
--                     xxx                                                | bld lv(1)| 7 - (level after build(1))
--                        x                                               | dbl mv   | 0: double move; 1: otherwise
--                         x                                              | mv op    | 0: opponent worker is pushed or swapped; 1: otherwise
--                                            x xxxx                      | op mv to | 0-24: opponnet worker move to
--                                                  x                     | op wid   | opponent worker id to be pushed or swapped; 0: worker 1; 1: worker 2
--                                                   xxx                  | bld (2)  | 0-24: index second build at
--                                                       xxxxx            | bld (1)  | 0-24: index first build at
--                                                            xxx xx      | mv to    | 0-24: index move to
--                                                                  xxxxx | mv from  | 0-24: index move from
--                                                                       x| wid      | 0: worker 1; 1: worker 2

