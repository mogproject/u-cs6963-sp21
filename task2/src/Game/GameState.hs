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
import Data.Bits (complement, countTrailingZeros, popCount, shift, xor, (.&.), (.|.))
import Data.Board (Board (Board))
import qualified Data.Board
import Data.Map (Map, (!))
import qualified Data.Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set
-- import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V

-- "vectorize" positions
type Index = Int -- 0 (row=1, col=1), 1 (row=1, col=2), ..., 24 (row=5, col=5)

type WorkerId = Int -- 0 or 1

type Players = [[Index]]

type Levels = Map Index Data.Board.Level

type LevelMap = Map Data.Board.Level Bitmap

type Turn = Int

type Bitmap = Int -- 25 bits

type GameMove = Int

type AdjList = Map Index Bitmap -- adjacency list from each index

type Score = Int

-- Internal representation of game states.
data GameState = GameState
  { players :: Players, -- Players (player to move, player to wait)
    levels :: Levels, -- Levels
    turn :: Turn,
    levelMap :: LevelMap, -- level [0..4] -> bitmap
    moveAdjacency :: AdjList, -- move adjacency list: index [0..24] -> bitmap
    buildAdjacency :: AdjList, -- build adjacency list: index [0..24] -> bitmap
    legalMoves :: [GameMove], -- Legal moves
    score :: Score -- Evaluation
  }
  deriving (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- Bit Operations
--------------------------------------------------------------------------------

fromList :: [Int] -> Bitmap
-- xs must be distinct and between 0 and 63.
fromList xs = sum [1 `shift` x | x <- xs]

toList :: Bitmap -> [Int]
toList x = tail . map fst $ takeWhile ((/= -1) . fst) $ iterate f (0, x)
  where
    f (_, y) =
      if y == 0
        then (-1, 0)
        else
          let p = countTrailingZeros y
           in (p, y `xor` (1 `shift` p))

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
      lm = createLevelMap lv
      adjM = createMoveAdjacencyList lv lm
      adjB = createBuildAdjacencyList lv lm
      mv = getLegalMoves pl adjM adjB
      sc = evaluate pl lv t adjM adjB mv
   in GameState {players = pl, levels = lv, turn = t, levelMap = lv, moveAdjacency = adjM, buildAdjacency = adjB, legalMoves = mv, score = sc}
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

defaultNeighbors :: V.Vector Index
defaultNeighbors = V.fromList [(fromList . filter (>= 0)) [f r c d | d <- [0 .. 7]] | r <- [0 .. 4], c <- [0 .. 4]]
  where
    rowOffset = V.fromList [-1, -1, 0, 1, 1, 1, 0, -1] :: V.Vector Int
    colOffset = V.fromList [0, 1, 1, 1, 0, -1, -1, -1] :: V.Vector Int
    f r c d =
      let rr = r + (rowOffset V.! d)
          cc = c + (colOffset V.! d)
       in if 0 <= rr && rr <= 4 && 0 <= cc && cc <= 4 then rr * 5 + cc else -1

createLevelMap :: Levels -> LevelMap
createLevelMap lv = Data.Map.fromList [(l, fromList [fst x | x <- Data.Map.toList lv, snd x == l]) | l <- [0 .. 4]]

createMoveAdjacencyList :: Levels -> LevelMap -> AdjList
createMoveAdjacencyList lv lm = Data.Map.fromList [(i, (defaultNeighbors V.! i) .&. sum [lm ! h | h <- [0 .. (min 3 (lv ! i) + 1)]]) | i <- [0 .. 24]]

createBuildAdjacencyList :: Levels -> LevelMap -> AdjList
createBuildAdjacencyList lv lm = Data.Map.fromList [(i, (defaultNeighbors V.! i) .&. sum [lm ! h | h <- [0 .. 3]]) | i <- [0 .. 24]]

encodeMove :: WorkerId -> Index -> Index -> Index -> GameMove
encodeMove wk mf mt bl = wk .|. (mf `shift` 1) .|. (mt `shift` 6) .|. (bl `shift` 11)

decodeMove :: GameMove -> (WorkerId, Index, Index, Index)
decodeMove m = (f 0 1, f 1 5, f 6 5, f 11 5)
  where
    f offset len = (m `shift` (- offset)) .&. ((1 `shift` len) - 1)

getLegalMoves :: Players -> AdjList -> AdjList -> [GameMove]
getLegalMoves pl adjM adjB = do
  wk <- [0, 1]
  let mf = pl !! 0 !! wk -- move from
  let ws = sum [(1 :: Int) `shift` x | x <- concat pl, x /= mf] -- bitmap of other workers
  let forbidden = complement ws

  mt <- toList $ (adjM ! mf) .&. forbidden -- move to
  bl <- toList $ (adjB ! mt) .&. forbidden -- build at
  return $ encodeMove wk mf mt bl

makeMove :: GameState -> GameMove -> GameState
makeMove
  GameState
    { players = [[w1, w2], p2],
      levels = lv,
      turn = t,
      levelMap = lm,
      moveAdjacency = adjM,
      buildAdjacency = adjB,
      legalMoves = mv,
      score = _
    }
  m =
    -- assume: m `elem` mv
    let (wk, _, mt, bl) = decodeMove m
        pl' = [p2, if wk == 0 then [mt, w2] else [w1, mt]]
        prevLevel = lv ! bl
        nextLevel = prevLevel + (if (lv ! mt) == 3 then 0 else 1) -- do not build after a winning move
        lv' = Data.Map.insert bl nextLevel lv
        t' = t + 1

        -- update level map
        f = Data.Map.adjust (xor (1 `shift` bl)) -- toggle bl bit
        lm' = f nextLevel $ f prevLevel lm

        -- update adjacency lists
        addArc m =
          if nextLevel <= 2 -- add arc: bl -> high N(bl)
            then Data.Map.adjust (.|. ((defaultNeighbors V.! bl) .&. (lm' ! (nextLevel + 1)))) bl m
            else m
        removeArc m =
          if prevLevel >= 1 -- remove arc: low N(bl) -> bl
            then foldl (flip f) m (toList (defaultNeighbors V.! bl .&. (lm' ! (prevLevel - 1))))
            else m

        adjM' =
          if nextLevel == 4
            then foldl (flip f) adjM (toList (adjB ! bl)) -- capped; we only care about the neighbors where we can build
            else (removeArc . addArc) adjM

        adjB' =
          if nextLevel == 4
            then foldl (flip f) adjB (toList (adjB ! bl)) -- capped
            else adjB

        mv' = getLegalMoves pl' adjM' adjB'
        sc' = evaluate pl' lv' t' adjM' adjB' mv'
     in GameState {players = pl', levels = lv', turn = t', levelMap = lm', moveAdjacency = adjM', buildAdjacency = adjB', legalMoves = mv', score = sc'}

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- Constants
scoreWin :: Score
scoreWin = 1000000000 -- 1e9

evalProxTable :: V.Vector Score
evalProxTable = V.fromList [0, 100, 80, 50, 20, 0]

evalProxTableSize :: Int
evalProxTableSize = V.length evalProxTable

getProxTableValue :: Int -> Score
getProxTableValue dist = evalProxTable V.! min (evalProxTableSize - 1) dist

evalReachTable :: V.Vector Score
evalReachTable =
  V.fromList $
    concat
      [ -- turn to move (advantageous)
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [300, 200, 40, 30, 20, 15, 12, 0], -- level 1
        [10000, 2000, 500, 200, 150, 120, 110, 0], -- level 2
        [scoreWin, 20000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0], -- level 4

        -- not turn to move
        [10, 9, 8, 7, 6, 5, 4, 0], -- level 0
        [300, 100, 40, 30, 20, 15, 12, 0], -- level 1
        [10000, 1000, 500, 200, 150, 120, 110, 0], -- level 2
        [scoreWin, 10000, 5000, 2000, 1000, 500, 300, 0], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0] -- level 4
      ]

evalReachTableSize :: Int
evalReachTableSize = V.length evalReachTable `div` 10

getReachTableValue :: Int -> Int -> Int -> Score
getReachTableValue player lev dist = evalReachTable V.! (player * evalReachTableSize * 5 + lev * evalReachTableSize + dist)

evalAsymTable :: V.Vector Score
evalAsymTable =
  V.fromList $
    concat
      [ [0, 0, 0, 1, 2, 3, 4, 5, 10], -- level 0
        [0, 0, 0, 10, 20, 30, 40, 50, 100], -- level 1
        [0, 100, 1000, 2000, 2000, 2000, 2000, 2000, 2000], -- level 2
        [0, 1000, 20000, 30000, 30000, 30000, 30000, 30000, 30000], -- level 3
        [0, 0, 0, 0, 0, 0, 0, 0, 0] -- level 4
      ]

evalAsymTableSize :: Int
evalAsymTableSize = V.length evalAsymTable `div` 5

getAsymTableValue :: Int -> Int -> Score
getAsymTableValue lev dist = evalAsymTable V.! (lev * evalAsymTableSize + min (evalAsymTableSize - 1) dist)

evalCornerBonus :: Score
evalCornerBonus = 20

evalStuckBonus :: Score
evalStuckBonus = 25000

evalPreventionAdvantage :: Score
evalPreventionAdvantage = 20000

evaluate :: Players -> Levels -> Turn -> AdjList -> AdjList -> [GameMove] -> Score
evaluate pl lv _ adjM adjB mv =
  if opponentWin
    then - scoreWin
    else
      let ps = [fromList ws | ws <- pl] -- player bitmaps
          dist = [[bfs adjM (complement (ps !! (1 - p))) (pl !! p !! w) | w <- [0, 1]] | p <- [0, 1]]
          bestDist = [V.fromList [minimum [dist !! p !! w ! i | w <- [0, 1]] | i <- [0 .. 24]] | p <- [0, 1]]
          funcs =
            [ evaluateWorkerProximity pl dist,
              evaluateReachability lv bestDist,
              evaluateAsymmetry lv adjB bestDist,
              evaluateStuckBonus pl adjM,
              evaluatePrevention pl lv adjB bestDist
            ]
       in sum [s * f p | (s, p) <- [(1, 0), (-1, 1)], f <- funcs]
  where
    -- FIXME: when cards are introduced
    opponentWin = null mv || elem 3 [lv ! i | i <- pl !! 1]

-- (1) Worker Proximity
evaluateWorkerProximity :: Players -> [[Map Index Int]] -> Int -> Score
evaluateWorkerProximity pl dist p = sum [getProxTableValue (dist !! p !! w ! (pl !! p !! (1 - w))) | w <- [0, 1]]

-- (2) Reachability
evaluateReachability :: Levels -> [V.Vector Int] -> Int -> Score
evaluateReachability lv dist p = sum [getReachTableValue p (lv ! i) (dist !! p V.! i) | i <- [0 .. 24]]

-- (3) Asymmetry
evaluateAsymmetry :: Levels -> AdjList -> [V.Vector Int] -> Int -> Score
evaluateAsymmetry lv adj dist p = sum [g p i | i <- [0 .. 24]]
  where
    cornerBonus i = (8 - popCount (adj ! i)) * (lv ! i) * evalCornerBonus
    g p i =
      let diff = (dist !! (1 - p) V.! i) - (dist !! p V.! i)
       in if diff > 0 then getAsymTableValue (lv ! i) diff else 0

-- (4) Stuck Bonus
evaluateStuckBonus :: Players -> AdjList -> Int -> Score
evaluateStuckBonus pl adj p = sum [if adj ! (pl !! p !! w) == 0 then evalStuckBonus else 0 | w <- [0, 1]]

-- (5) Prevension
evaluatePrevention :: Players -> Levels -> AdjList -> [V.Vector Int] -> Int -> Score
evaluatePrevention pl lv adj dist p = sum [f index | w <- [0, 1], let index = pl !! p !! w, lv ! index == 2]
  where
    f index = if minimum [dist !! (1 - p) V.! u | u <- toList (adj ! index)] >= 2 then evalPreventionAdvantage else 0

--------------------------------------------------------------------------------
-- Distance Computation
--------------------------------------------------------------------------------

distInf :: Int
distInf = 100 -- infinity distance

-- breadth-first search
bfs :: AdjList -> Bitmap -> Index -> Map Index Int
bfs adj avail start =
  let initMap = Data.Map.fromList [(i, if i == start then 0 else distInf) | i <- [0 .. 24]]
   in bfs' adj avail (Seq.singleton start) initMap

bfs' :: AdjList -> Bitmap -> Seq Index -> Map Index Int -> Map Index Int
bfs' adj avail q sofar | Seq.null q = sofar
bfs' adj avail q sofar =
  let x = q `Seq.index` 0
      nbrs = toList $ (adj ! x) .&. avail
      unseen = Seq.fromList [nbr | nbr <- nbrs, (sofar ! nbr) == distInf]
      d = (sofar ! x) + 1
      q' = Seq.drop 1 q Seq.>< unseen
      sofar' = foldl (\m u -> Data.Map.insert u d m) sofar unseen
   in bfs' adj avail q' sofar'
