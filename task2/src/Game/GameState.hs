module Game.GameState
  ( GameState (GameState),
    GameMove,
    fromBoard,
    toBoard,
    makeMove,
    players,
    levels,
    turn,
    legalMoves,
    score,
  )
where

import Control.Monad (guard)
import Data.Bits (shift, (.&.), (.|.))
import Data.Board (Board (Board))
import qualified Data.Board
import Data.Map (Map, (!))
import qualified Data.Map
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V

type Index = Int -- 0 (row=1, col=1), 1 (row=1, col=2), ..., 24 (row=5, col=5)

type WorkerId = Int -- 0 or 1

type Players = [[Index]]

type Levels = Map Index Data.Board.Level

type Turn = Int

-- type Direction = Int -- 0 (north), 1 (north east), ..., 7 (north west)

type GameMove = Int

type Score = Int

-- Internal representation of game states.
data GameState = GameState
  { players :: Players, -- Players (player to move, player to wait)
    levels :: Levels, -- Levels
    turn :: Turn,
    legalMoves :: [GameMove], -- Legal moves
    score :: Score -- Evaluation
  }
  deriving (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

toIndex :: Data.Board.Pos -> Index
toIndex (r, c) = (r - 1) * 5 + (c - 1)

fromIndex :: Index -> Data.Board.Pos
fromIndex i = (i `div` 5 + 1, i `mod` 5 + 1)

fromBoard :: Board -> GameState
fromBoard Board {Data.Board.players = [(w1, w2), (w3, w4)], Data.Board.spaces = sp, Data.Board.turn = t} =
  let pl = [[toIndex w1, toIndex w2], [toIndex w3, toIndex w4]]
      lv = Data.Map.fromList . zip [0 .. 24] $ concat sp
      mv = getLegalMoves pl lv
      sc = evaluate pl lv t mv
   in GameState {players = pl, levels = lv, turn = t, legalMoves = mv, score = sc}
fromBoard _ = undefined

toBoard :: GameState -> Board
toBoard GameState {players = [[w1, w2], [w3, w4]], levels = lv, turn = t, legalMoves = _, score = _} =
  let pl = [(fromIndex w1, fromIndex w2), (fromIndex w3, fromIndex w4)]
      sp = [[lv ! (r * 5 + c) | c <- [0 .. 4]] | r <- [0 .. 4]]
   in Board {Data.Board.players = pl, Data.Board.spaces = sp, Data.Board.turn = t}
toBoard _ = undefined

--------------------------------------------------------------------------------
-- Moves
--------------------------------------------------------------------------------

neighbors :: VB.Vector (V.Vector Index)
neighbors = VB.fromList [V.fromList [f r c d | d <- [0 .. 7]] | r <- [0 .. 4], c <- [0 .. 4]]
  where
    rowOffset = V.fromList [-1, -1, 0, 1, 1, 1, 0, -1] :: V.Vector Int
    colOffset = V.fromList [0, 1, 1, 1, 0, -1, -1, -1] :: V.Vector Int
    f r c d =
      let rr = r + (rowOffset V.! d)
          cc = c + (colOffset V.! d)
       in if 0 <= rr && rr <= 4 && 0 <= cc && cc <= 4 then rr * 5 + cc else -1

encodeMove :: WorkerId -> Index -> Index -> Index -> GameMove
encodeMove wk mf mt bl = wk .|. (mf `shift` 1) .|. (mt `shift` 6) .|. (bl `shift` 11)

decodeMove :: GameMove -> (WorkerId, Index, Index, Index)
decodeMove m = (f 0 1, f 1 5, f 6 5, f 11 5)
  where
    f offset len = (m `shift` (- offset)) .&. ((1 `shift` len) - 1)

getLegalMoves :: Players -> Levels -> [GameMove]
getLegalMoves pl lv = do
  wk <- [0, 1]
  let mf = pl !! 0 !! wk -- move from
  let ws = [x | x <- concat pl, x /= mf] -- other workers

  -- move
  md <- [0 .. 7]
  let mt = neighbors VB.! mf V.! md -- move to
  guard $ mt >= 0 -- on board
  guard $ (lv ! mt) < 4 -- not capped
  guard $ (lv ! mf) + 1 >= (lv ! mt) -- go up no more than one level
  guard $ notElem mt ws -- no other workers

  -- build
  bd <- [0 .. 7]
  let bl = neighbors VB.! mt V.! bd -- build at
  guard $ bl >= 0 -- on board
  guard $ (lv ! bl) < 4 -- not capped
  guard $ notElem bl ws -- no other workers
  return $ encodeMove wk mf mt bl

makeMove :: GameState -> GameMove -> GameState
makeMove (GameState {players = [[w1, w2], p2], levels = lv, turn = t, legalMoves = mv, score = _}) m
  | elem m mv =
    let (wk, _, mt, bl) = decodeMove m
        pl' = [p2, if wk == 0 then [mt, w2] else [w1, mt]]
        lv' = if (lv ! mt) < 3 then Data.Map.adjust (1 +) bl lv else lv
        t' = t + 1
        mv' = getLegalMoves pl' lv'
        sc' = evaluate pl' lv' t' mv'
     in GameState {players = pl', levels = lv', turn = t', legalMoves = mv', score = sc'}
makeMove _ _ = undefined

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------
scoreWin :: Score
scoreWin = 1000000000 -- 1e9

evaluate :: Players -> Levels -> Turn -> [GameMove] -> Score
evaluate pl lv _ mv =
  if null mv
    then - scoreWin
    else -- FIXME: when cards are introduced

      if any (== 3) [lv ! i | i <- (head pl)]
        then scoreWin
        else
          if any (== 3) [lv ! i | i <- (pl !! 1)]
            then - scoreWin
            else 0
