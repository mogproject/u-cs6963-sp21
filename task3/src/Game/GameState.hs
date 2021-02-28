{-# LANGUAGE MultiWayIf #-}

module Game.GameState
  ( GameState (GameState),
    GameMove,
    fromBoard,
    toBoard,
    fromBoardWithoutCard,
    toBoardWithoutCard,
    makeMove,
    cards,
    players,
    levels,
    turn,
    levelMap,
    moveAdjacency,
    buildAdjacency,
    legalMoves,
    Index,
    Cards,
    Players,
    Levels,
    Bitmap,
    AdjList,
    toList,
    fromList,
  )
where

import Data.Bits (complement, countTrailingZeros, shift, xor, (.&.), (.|.))
import Data.Board (Board (Board), Level, Player (Player))
import qualified Data.Board
import qualified Data.BoardWithoutCard
import Data.Card (Card (Apollo, Artemis, Atlas, Demeter, Hephastus, Minotaur, Pan, Prometheus))
import Data.List (sort, tails)
import Data.Map (Map, (!))
import qualified Data.Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import Game.GameMove

-- "vectorize" positions
type Index = Int -- 0 (row=1, col=1), 1 (row=1, col=2), ..., 24 (row=5, col=5)

type WorkerId = Int -- 0 or 1

type Cards = [Maybe Card]

type Players = [[Index]]

type Levels = Map Index Level

type LevelMap = Map Level Bitmap

type Turn = Int

type Bitmap = Int -- 25 bits

type AdjList = Map Index Bitmap -- adjacency list from each index

-- Internal representation of game states.
data GameState = GameState
  { cards :: Cards, -- Cards (player to move, player to wait)
    players :: Players, -- Players (player to move, player to wait)
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

bitElem :: Int -> Bitmap -> Bool
bitElem i x = (x `shift` (- i)) .&. 1 == 1

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

toIndex :: Data.Board.Pos -> Index
toIndex (r, c) = (r - 1) * 5 + (c - 1)

fromIndex :: Index -> Data.Board.Pos
fromIndex i = (i `div` 5 + 1, i `mod` 5 + 1)

fromBoard' :: Maybe Card -> Maybe Card -> Data.Board.Workers -> Data.Board.Workers -> [[Data.Board.Level]] -> Int -> GameState
fromBoard' c1 c2 (w1, w2) (w3, w4) sp t =
  let cs = [c1, c2]
      pl = [[toIndex w1, toIndex w2], [toIndex w3, toIndex w4]]
      lv = Data.Map.fromList . zip [0 .. 24] $ concat sp
      lm = createLevelMap lv
      adjM = createMoveAdjacencyList lv lm
      adjB = createBuildAdjacencyList lv lm
      mv = getLegalMoves cs pl lv lm adjM adjB
   in GameState {cards = cs, players = pl, levels = lv, turn = t, levelMap = lm, moveAdjacency = adjM, buildAdjacency = adjB, legalMoves = mv}

fromBoard :: Board -> GameState
fromBoard
  Board
    { Data.Board.players =
        ( Player {Data.Board.card = c1, Data.Board.tokens = Just w1},
          Player {Data.Board.card = c2, Data.Board.tokens = Just w2}
          ),
      Data.Board.spaces = sp,
      Data.Board.turn = t
    } = fromBoard' (Just c1) (Just c2) w1 w2 sp t
fromBoard _ = undefined

fromBoardWithoutCard :: Data.BoardWithoutCard.Board -> GameState
fromBoardWithoutCard Data.BoardWithoutCard.Board {Data.BoardWithoutCard.players = [w1, w2], Data.BoardWithoutCard.spaces = sp, Data.BoardWithoutCard.turn = t} =
  fromBoard' Nothing Nothing w1 w2 sp t
fromBoardWithoutCard _ = undefined

toBoard :: GameState -> Board
toBoard GameState {cards = [Just c1, Just c2], players = [[w1, w2], [w3, w4]], levels = lv, turn = t} =
  let pl =
        ( Player {Data.Board.card = c1, Data.Board.tokens = Just (fromIndex w1, fromIndex w2)},
          Player {Data.Board.card = c2, Data.Board.tokens = Just (fromIndex w3, fromIndex w4)}
        )
      sp = [[lv ! (r * 5 + c) | c <- [0 .. 4]] | r <- [0 .. 4]]
   in Board {Data.Board.players = pl, Data.Board.spaces = sp, Data.Board.turn = t}
toBoard _ = undefined

toBoardWithoutCard :: GameState -> Data.BoardWithoutCard.Board
toBoardWithoutCard GameState {players = [[w1, w2], [w3, w4]], levels = lv, turn = t} =
  let pl = [(fromIndex w1, fromIndex w2), (fromIndex w3, fromIndex w4)]
      sp = [[lv ! (r * 5 + c) | c <- [0 .. 4]] | r <- [0 .. 4]]
   in Data.BoardWithoutCard.Board {Data.BoardWithoutCard.players = pl, Data.BoardWithoutCard.spaces = sp, Data.BoardWithoutCard.turn = t}
toBoardWithoutCard _ = undefined

--------------------------------------------------------------------------------
-- Moves
--------------------------------------------------------------------------------

defaultNeighbors :: V.Vector Bitmap
defaultNeighbors = V.fromList [(fromList . filter (>= 0)) [f r c d | d <- [0 .. 7]] | r <- [0 .. 4], c <- [0 .. 4]]
  where
    rowOffset = V.fromList [-1, -1, 0, 1, 1, 1, 0, -1] :: V.Vector Int
    colOffset = V.fromList [0, 1, 1, 1, 0, -1, -1, -1] :: V.Vector Int
    f r c d =
      let rr = r + (rowOffset V.! d)
          cc = c + (colOffset V.! d)
       in if 0 <= rr && rr <= 4 && 0 <= cc && cc <= 4 then rr * 5 + cc else -1

-- used for Minotaur's move
pushToTable :: V.Vector Index
pushToTable = V.fromList [f mf mt | mf <- [0 .. 24], mt <- [0 .. 24]]
  where
    f moveFrom moveTo =
      let pushTo = moveTo * 2 - moveFrom
          nbrs = defaultNeighbors V.! moveTo
          isValid = 0 <= pushTo && pushTo <= 24 && moveFrom `bitElem` nbrs && pushTo `bitElem` nbrs
       in if isValid then pushTo else -1

lookupPushTo :: Index -> Index -> Index
lookupPushTo moveFrom moveTo = pushToTable V.! (moveFrom * 25 + moveTo)

createLevelMap :: Levels -> LevelMap
createLevelMap lv = Data.Map.fromList [(l, fromList [fst x | x <- Data.Map.toList lv, snd x == l]) | l <- [0 .. 4]]

createMoveAdjacencyList :: Levels -> LevelMap -> AdjList
createMoveAdjacencyList lv lm = Data.Map.fromList [(i, if lv ! i == 4 then 0 else (defaultNeighbors V.! i) .&. sum [lm ! h | h <- [0 .. (min 3 ((lv ! i) + 1))]]) | i <- [0 .. 24]]

createBuildAdjacencyList :: Levels -> LevelMap -> AdjList
createBuildAdjacencyList lv lm = Data.Map.fromList [(i, if lv ! i == 4 then 0 else (defaultNeighbors V.! i) .&. sum [lm ! h | h <- [0 .. 3]]) | i <- [0 .. 24]]

--------------------------------------------------------------------------------
-- Move to
--------------------------------------------------------------------------------
getLegalMoveTo :: Maybe Card -> Index -> Level -> LevelMap -> Bitmap -> Bitmap -> Bitmap -> AdjList -> [Index]
--
-- [Artemis]
-- The moved token can optionally move a second time (i.e., the same token),
-- as long as the first move doesn’t win, and as long as the second move doesn’t return
-- to the original space.
getLegalMoveTo (Just Artemis) mf mfl lm _ allWorkers _ adjM =
  let firstMove = (adjM ! mf) .&. complement allWorkers
      secondMoveFrom = (if mfl == 2 then (complement (lm ! 3) .&.) else id) firstMove
   in toList $ foldl (\z x -> z .|. adjM ! x) firstMove (toList secondMoveFrom) .&. complement allWorkers
--
-- [Minotaur]
-- A token’s move can optionally enter the space of an opponent’s token,
-- but only if the token can be pushed back to an unoccupied space, and only as long as
-- the token would be able to move to the opponent’s space if the opponent token were not there.
-- The unoccupied space where the opponent’s token is pushed can be at any level less than 4.
getLegalMoveTo (Just Minotaur) mf _ _ friend _ emptySpace adjM =
  let candidates = (adjM ! mf) .&. complement friend
      isValidPushTo pt = pt /= -1 && pt `bitElem` emptySpace
   in filter (isValidPushTo . lookupPushTo mf) (toList candidates)
--
-- [Apollo]
-- A token’s move can optionally swap places with an adjacent opponent token,
-- as long as the token would be able to move to the opponent’s space if the
-- opponent token were not there; otherwise, the move must be to an unoccupied space as usual.
getLegalMoveTo (Just Apollo) mf _ _ friend _ _ adjM = toList $ (adjM ! mf) .&. complement friend
--
-- Others.
getLegalMoveTo _ mf _ _ _ allWorkers _ adjM = toList $ (adjM ! mf) .&. complement allWorkers

--------------------------------------------------------------------------------
-- Push to
--------------------------------------------------------------------------------
getLegalPushTo :: Maybe Card -> Players -> Index -> Index -> Maybe (WorkerId, Index)
getLegalPushTo (Just Apollo) pl mf mt | pl !! 1 !! 0 == mt = Just (0, mf)
getLegalPushTo (Just Apollo) pl mf mt | pl !! 1 !! 1 == mt = Just (1, mf)
getLegalPushTo (Just Minotaur) pl mf mt | pl !! 1 !! 0 == mt = Just (0, lookupPushTo mf mt)
getLegalPushTo (Just Minotaur) pl mf mt | pl !! 1 !! 1 == mt = Just (1, lookupPushTo mf mt)
getLegalPushTo _ _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Build at
--------------------------------------------------------------------------------

-- taken from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | y : xs' <- tails xs, ys <- combinations (n -1) xs']

getLegalBuildAt :: Maybe Card -> Levels -> AdjList -> Bitmap -> Index -> Index -> [[(Index, Level)]]
--
-- [Atlas]
-- The build phase can build a space currently at level 0, 1, 2 to make it level 4,
-- instead of building to exactly one more than the space’s current level.
getLegalBuildAt (Just Atlas) lv adjB emptySpace _ mt =
  let hs l = if l == 3 then [4] else [l + 1, 4]
   in [[(bl, h)] | bl <- toList $ (adjB ! mt) .&. emptySpace, h <- hs (lv ! bl)]
--
-- [Demeter]
-- The moved token can optionally build a second time, but not on the same space
-- as the first build within a turn.
getLegalBuildAt (Just Demeter) lv adjB emptySpace _ mt =
  let cand = toList $ (adjB ! mt) .&. emptySpace
      comb = [[x] | x <- cand] ++ combinations 2 cand
   in [[(x, (lv ! x) + 1) | x <- xs] | xs <- comb]
--
-- [Hephastus]
-- The moved token can optionally build a second time, but only on the same space
-- as the first build within a turn, and only if the second build does not reach level 4.
getLegalBuildAt (Just Hephastus) lv adjB emptySpace _ mt =
  let hs l = if l <= 1 then [l + 1, l + 2] else [l + 1]
   in [[(bl, h)] | bl <- toList $ (adjB ! mt) .&. emptySpace, h <- hs (lv ! bl)]
--
-- [Prometheus]
-- A token can optionally build before moving, but then the move is constrained to
-- the same level or lower (i.e., the level of the token’s new space can be no larger
-- than the level of the token’s old space).
getLegalBuildAt (Just Prometheus) lv adjB emptySpace mf mt
  | (lv ! mf) >= (lv ! mt) =
    let secondBuild = toList $ (adjB ! mt) .&. emptySpace
        firstBuild = toList $ (adjB ! mf) .&. emptySpace .&. complement (1 `shift` mt)
     in [ if x == y then [(x, (lv ! x) + 2)] else [(x, (lv ! x) + 1), (y, (lv ! y) + 1)]
          | x <- secondBuild,
            y <- firstBuild,
            x /= y || lv ! x <= 2
        ]
--
-- Others.
getLegalBuildAt _ lv adjB emptySpace _ mt = [[(bl, (lv ! bl) + 1)] | bl <- toList $ (adjB ! mt) .&. emptySpace]

--------------------------------------------------------------------------------
-- All legal moves
--------------------------------------------------------------------------------

getLegalMoves :: Cards -> Players -> Levels -> LevelMap -> AdjList -> AdjList -> [GameMove]
getLegalMoves (c1 : _) pl lv lm adjM adjB = sort $ do
  wk <- [0, 1]
  let mf = pl !! 0 !! wk -- move from
  let mfl = lv ! mf -- move from level

  -- bitmaps
  let opponentWorkers = sum [1 `shift` x | x <- pl !! 1] :: Int
  let friendWorker = 1 `shift` (pl !! 0 !! (1 - wk)) :: Int
  let otherWorkers = opponentWorkers .|. friendWorker
  let allWorkers = otherWorkers .|. (1 `shift` (pl !! 0 !! wk))
  let emptySpace = ((1 `shift` 25) - 1) .&. complement ((lm ! 4) .|. otherWorkers) :: Bitmap

  -- move to
  mt <- getLegalMoveTo c1 mf mfl lm friendWorker allWorkers emptySpace adjM
  let mtl = lv ! mt -- move to level
  let applyDoubleMove = if mt `bitElem` (adjM ! mf) then id else setDoubleMove

  -- push to
  let pushInfo = getLegalPushTo c1 pl mf mt
  let applyPushTo = maybe id (uncurry setOpponentMove) pushInfo

  -- check point 1
  let moveSofar = (applyPushTo . applyDoubleMove . setMoveToLevel mtl . setMoveTo mt . setMoveFrom mf . setWorkerId wk) createGameMove
  let emptySofar = emptySpace .&. complement (maybe 0 (\(_, pushTo) -> 1 `shift` pushTo) pushInfo)

  -- check winning
  -- Note: it's possible for Artemis to win by moving from level 1
  if (mfl < 3 && mtl == 3) || (Pan `elem` c1 && mtl + 2 <= mfl)
    then return $ setWin moveSofar
    else do
      -- move to (second)
      bls <- getLegalBuildAt c1 lv adjB emptySofar mf mt

      -- check point 2
      let moveSofar' = setBuildAt bls moveSofar

      -- move evaluation
      -- TODO: Implement
      return moveSofar'
getLegalMoves _ _ _ _ _ _ = undefined

makeMove :: GameState -> GameMove -> GameState
makeMove
  GameState
    { cards = [c1, c2],
      players = [p1, p2],
      levels = lv,
      turn = t,
      levelMap = lm,
      moveAdjacency = adjM,
      buildAdjacency = adjB,
      legalMoves = _
    }
  mv =
    -- assume: m `elem` mv
    let wk = getWorkerId mv
        mt = getMoveTo mv
        (owid, omt) = getOpponentMove mv
        builds = getBuildAt mv

        -- turn
        t' = t + 1

        -- cards
        cs' = [c2, c1]

        -- players
        p1' = [if wk == i then mt else p1 !! i | i <- [0, 1]]
        p2' = [if owid == i then omt else p2 !! i | i <- [0, 1]]
        pl' = [p2', p1']

        (lv', lm', adjM', adjB') =
          foldl
            ( \(xlv, xlm, xadjM, xadjB) (bl, nextLevel) ->
                let prevLevel = xlv ! bl
                    f = Data.Map.adjust (xor (1 `shift` bl)) -- toggle bl bit
                    f' = Data.Map.adjust (.&. complement (1 `shift` bl)) -- remove bl bit
                    g lo hi = foldl (.|.) 0 [xlm ! h | h <- [lo .. hi]] -- merge level range

                    -- levels
                    xlv' = Data.Map.insert bl nextLevel xlv
                    -- level map
                    xlm' = f nextLevel $ f prevLevel xlm

                    -- update adjacency lists
                    addArc m =
                      if prevLevel <= 1 -- add arc: bl -> high N(bl)
                        then Data.Map.adjust (.|. ((defaultNeighbors V.! bl) .&. g (prevLevel + 2) (min 3 (nextLevel + 1)))) bl m
                        else m
                    removeArc m =
                      if nextLevel >= 2 -- remove arc: low N(bl) -> bl
                        then foldl (flip f) m (toList ((defaultNeighbors V.! bl) .&. g (max 0 (prevLevel - 1)) (nextLevel - 2)))
                        else m

                    xadjM'
                      | prevLevel == nextLevel = xadjM
                      | nextLevel == 4 = foldl (flip f') (Data.Map.insert bl 0 xadjM) (toList (xadjB ! bl)) -- capped
                      | otherwise = (removeArc . addArc) xadjM

                    xadjB'
                      | nextLevel == 4 = foldl (flip f') (Data.Map.insert bl 0 xadjB) (toList (xadjB ! bl)) -- capped
                      | otherwise = xadjB
                 in (xlv', xlm', xadjM', xadjB')
            )
            (lv, lm, adjM, adjB)
            builds

        mv' = getLegalMoves cs' pl' lv' lm' adjM' adjB'
     in GameState {cards = cs', players = pl', levels = lv', turn = t', levelMap = lm', moveAdjacency = adjM', buildAdjacency = adjB', legalMoves = mv'}
makeMove _ _ = undefined
