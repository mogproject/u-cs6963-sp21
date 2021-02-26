module Game.GameState
  ( GameState (GameState),
    GameMove,
    fromBoard,
    toBoard,
    makeMove,
    players,
    levels,
    turn,
    levelMap,
    moveAdjacency,
    buildAdjacency,
    legalMoves,
    Index,
    Players,
    Levels,
    Bitmap,
    AdjList,
    toList,
    fromList,
    decodeMove,
  )
where

import Data.Bits (complement, countTrailingZeros, shift, xor, (.&.), (.|.))
import Data.Board (Board (Board), Player (Player))
import qualified Data.Board
import Data.Map (Map, (!))
import qualified Data.Map
import qualified Data.Vector.Unboxed as V
import Data.Card (Card (Apollo))

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

-- Internal representation of game states.
data GameState = GameState
  { players :: Players, -- Players (player to move, player to wait)
    levels :: Levels, -- Levels
    turn :: Turn,
    levelMap :: LevelMap, -- level [0..4] -> bitmap
    moveAdjacency :: AdjList, -- move adjacency list: index [0..24] -> bitmap
    buildAdjacency :: AdjList, -- build adjacency list: index [0..24] -> bitmap
    legalMoves :: [GameMove] -- Legal moves
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
fromBoard Board {Data.Board.players = (Player {Data.Board.tokens = Just (w1, w2)}, Player {Data.Board.tokens = Just (w3, w4)}), Data.Board.spaces = sp, Data.Board.turn = t} =
  let pl = [[toIndex w1, toIndex w2], [toIndex w3, toIndex w4]]
      lv = Data.Map.fromList . zip [0 .. 24] $ concat sp
      lm = createLevelMap lv
      adjM = createMoveAdjacencyList lv lm
      adjB = createBuildAdjacencyList lv lm
      mv = getLegalMoves pl adjM adjB
   in GameState {players = pl, levels = lv, turn = t, levelMap = lm, moveAdjacency = adjM, buildAdjacency = adjB, legalMoves = mv}
fromBoard _ = undefined

toBoard :: GameState -> Board
toBoard GameState {players = [[w1, w2], [w3, w4]], levels = lv, turn = t, legalMoves = _} =
  let pl =
        ( Player {Data.Board.card = Apollo, Data.Board.tokens = Just (fromIndex w1, fromIndex w2)},
          Player {Data.Board.card = Apollo, Data.Board.tokens = Just (fromIndex w3, fromIndex w4)}
        )
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
createMoveAdjacencyList lv lm = Data.Map.fromList [(i, if lv ! i == 4 then 0 else (defaultNeighbors V.! i) .&. sum [lm ! h | h <- [0 .. (min 3 ((lv ! i) + 1))]]) | i <- [0 .. 24]]

createBuildAdjacencyList :: Levels -> LevelMap -> AdjList
createBuildAdjacencyList lv lm = Data.Map.fromList [(i, if lv ! i == 4 then 0 else (defaultNeighbors V.! i) .&. sum [lm ! h | h <- [0 .. 3]]) | i <- [0 .. 24]]

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
      legalMoves = _
    }
  mv =
    -- assume: m `elem` mv
    let (wk, _, mt, bl) = decodeMove mv
        pl' = [p2, if wk == 0 then [mt, w2] else [w1, mt]]
        prevLevel = lv ! bl
        nextLevel = prevLevel + (if (lv ! mt) == 3 then 0 else 1) -- do not build after a winning move
        lv' = Data.Map.insert bl nextLevel lv
        t' = t + 1

        -- update level map
        f = Data.Map.adjust (xor (1 `shift` bl)) -- toggle bl bit
        f' = Data.Map.adjust (.&. complement (1 `shift` bl)) -- remove bl bit
        lm' = f nextLevel $ f prevLevel lm

        -- update adjacency lists
        addArc m =
          if nextLevel <= 2 -- add arc: bl -> high N(bl)
            then Data.Map.adjust (.|. ((defaultNeighbors V.! bl) .&. (lm ! (nextLevel + 1)))) bl m
            else m
        removeArc m =
          if nextLevel >= 2 -- remove arc: low N(bl) -> bl
            then foldl (flip f) m (toList ((defaultNeighbors V.! bl) .&. (lm ! (nextLevel - 2))))
            else m

        adjM'
          | prevLevel == nextLevel = adjM
          | nextLevel == 4 = foldl (flip f') (Data.Map.insert bl 0 adjM) (toList (adjB ! bl)) -- capped
          | otherwise = (removeArc . addArc) adjM

        adjB'
          | prevLevel == nextLevel = adjB
          | nextLevel == 4 = foldl (flip f') (Data.Map.insert bl 0 adjB) (toList (adjB ! bl)) -- capped
          | otherwise = adjB

        mv' = getLegalMoves pl' adjM' adjB'
     in GameState {players = pl', levels = lv', turn = t', levelMap = lm', moveAdjacency = adjM', buildAdjacency = adjB', legalMoves = mv'}
makeMove _ _ = undefined
