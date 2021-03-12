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
    playerMap,
    levels,
    turn,
    levelMap,
    legalMoves,
    Index,
    Cards,
    Players,
    PlayerMap,
    Levels,
    LevelMap,
    getLegalMoves',
    getLegalMoveTo,
    getLegalMoveTo',
    getLegalBuildAt,
    getLegalPushTo,
    createPlayerMap,
    createLevelMap,
    isWinningMove,
    hasWinningMove,
    makeNextLevelMap,
  )
where

import Control.Monad (guard)
import Data.Bits (complement, shift, xor, (.&.), (.|.))
import Data.Board (Board (Board), Level, Player (Player))
import qualified Data.Board
import qualified Data.BoardWithoutCard
import Data.Card (Card (Apollo, Artemis, Atlas, Demeter, Hephastus, Minotaur, Pan, Prometheus))
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as Map
import Data.List (find, foldl', sort, tails)
import Game.BitBoard
import Game.GameMove

type WorkerId = Int -- 0 or 1

type Cards = [Maybe Card]

type Players = [[Index]]

type PlayerMap = IntMap BitBoard

type Turn = Int

type Levels = IntMap Level

type LevelMap = [BitBoard]

-- Internal representation of game states.
data GameState = GameState
  { cards :: Cards, -- Cards (player to move, player to wait)
    players :: Players, -- Players (player to move, player to wait)
    -- Player Map: bitboards for each player and its combinations
    --   0 -> Player 1 Worker 1
    --   1 -> Player 1 Worker 2
    --   2 -> Player 2 Worker 1
    --   3 -> Player 2 Worker 2
    --   4 -> Player 1 both workers
    --   5 -> Player 2 both workers
    --   6 -> All workers
    playerMap :: PlayerMap,
    levels :: Levels, -- Levels: Index -> Level
    -- Level Map: bitboards for each level and its combinations
    --   0 -> Level 0
    --   1 -> Level 1
    --   2 -> Level 2
    --   3 -> Level 3
    --   4 -> Level 4
    --   5 -> Level 0 or 1
    --   6 -> Level 0, 1 or 2
    --   7 -> Level 0, 1, 2 or 3
    levelMap :: LevelMap,
    turn :: Turn,
    legalMoves :: [GameMove] -- Legal moves
  }
  deriving (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

fromBoard' :: Maybe Card -> Maybe Card -> Data.Board.Workers -> Data.Board.Workers -> [[Data.Board.Level]] -> Int -> GameState
fromBoard' c1 c2 (w1, w2) (w3, w4) sp t =
  let cs = [c1, c2]
      pl = [[posToIndex w1, posToIndex w2], [posToIndex w3, posToIndex w4]]
      pm = createPlayerMap pl
      lv = Map.fromList [(posToIndex (r, c), sp !! (r -1) !! (c -1)) | r <- [1 .. 5], c <- [1 .. 5]]
      lm = createLevelMap lv
      mv = getLegalMoves True cs pl pm lv lm
   in GameState {cards = cs, players = pl, playerMap = pm, levels = lv, turn = t, levelMap = lm, legalMoves = mv}

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
        ( Player {Data.Board.card = c1, Data.Board.tokens = Just (indexToPos w1, indexToPos w2)},
          Player {Data.Board.card = c2, Data.Board.tokens = Just (indexToPos w3, indexToPos w4)}
        )
      sp = [[lv ! posToIndex (r, c) | c <- [1 .. 5]] | r <- [1 .. 5]]
   in Board {Data.Board.players = pl, Data.Board.spaces = sp, Data.Board.turn = t}
toBoard _ = undefined

toBoardWithoutCard :: GameState -> Data.BoardWithoutCard.Board
toBoardWithoutCard GameState {players = [[w1, w2], [w3, w4]], levels = lv, turn = t} =
  let pl = [(indexToPos w1, indexToPos w2), (indexToPos w3, indexToPos w4)]
      sp = [[lv ! posToIndex (r, c) | c <- [1 .. 5]] | r <- [1 .. 5]]
   in Data.BoardWithoutCard.Board {Data.BoardWithoutCard.players = pl, Data.BoardWithoutCard.spaces = sp, Data.BoardWithoutCard.turn = t}
toBoardWithoutCard _ = undefined

createPlayerMap :: Players -> PlayerMap
createPlayerMap pl =
  let m = [singletonBB (pl !! p !! w) | p <- [0, 1], w <- [0, 1]]
      v4 = (m !! 0) .|. (m !! 1)
      v5 = (m !! 2) .|. (m !! 3)
      v6 = v4 .|. v5
   in Map.fromList $ zip [0 ..] (m ++ [v4, v5, v6])

createLevelMap :: Levels -> LevelMap
-- createLevelMap lv =
--   let m' = Map.fromListWith (.|.) [(v, singletonBB k) | (k, v) <- Map.toList lv]
--       m = Map.union m' (Map.fromList [(l, 0) | l <- [0 .. 4]])
--       v5 = (m ! 0) .|. (m ! 1) -- level 0 or 1
--       v6 = v5 .|. (m ! 2) -- level 0, 1 or 2
--       v7 = v6 .|. (m ! 3) -- level 0, 1, 2 or 3
--    in Map.insert 5 v5 $ Map.insert 6 v6 $ Map.insert 7 v7 m

createLevelMap lv =
  let (v0, v1, v2, v3, v4) =
        foldl'
          ( \(x0, x1, x2, x3, x4) (i, y) ->
              let bb = singletonBB i
               in case y of
                    0 -> (x0 + bb, x1, x2, x3, x4)
                    1 -> (x0, x1 + bb, x2, x3, x4)
                    2 -> (x0, x1, x2 + bb, x3, x4)
                    3 -> (x0, x1, x2, x3 + bb, x4)
                    _ -> (x0, x1, x2, x3, x4 + bb)
          )
          (0, 0, 0, 0, 0)
          (Map.toList lv)
      v5 = v0 + v1 -- level 0 or 1
      v6 = v5 + v2 -- level 0, 1 or 2
      v7 = v6 + v3 -- level 0, 1, 2 or 3
   in [v0, v1, v2, v3, v4, v5, v6, v7]

--------------------------------------------------------------------------------
-- Move to
--------------------------------------------------------------------------------
getLegalMoveTo' :: Index -> [BitBoard] -> BitBoard
getLegalMoveTo' mf lm = getNeighborhood mf .&. canMoveTo mf
  where
    canMoveTo i
      | i `elemBB` (lm !! 0) = lm !! 5
      | i `elemBB` (lm !! 1) = lm !! 6
      | otherwise = lm !! 7

getLegalMoveTo :: Maybe Card -> Index -> PlayerMap -> LevelMap -> [Index]
--
-- [Artemis]
-- The moved token can optionally move a second time (i.e., the same token),
-- as long as the first move doesn’t win, and as long as the second move doesn’t return
-- to the original space.

getLegalMoveTo (Just Artemis) mf pm lm =
  let firstMove = getLegalMoveTo' mf lm `andNotBB` (pm ! 6)
      secondMoveFrom = (if mf `elemBB` (lm !! 2) then (\x -> x `andNotBB` (lm !! 3)) else id) firstMove
   in bbToList $ foldl' (\z x -> z .|. getLegalMoveTo' x lm) firstMove (bbToList secondMoveFrom) `andNotBB` (pm ! 6)
--
-- =========================================================================================
--     Less efficient implementation
-- =========================================================================================
-- Bitboard-based legal move-to computation
-- getLegalMoveTo'' :: LevelMap -> BitBoard -> (BitBoard, BitBoard, BitBoard, BitBoard) -> ((BitBoard, BitBoard, BitBoard, BitBoard), BitBoard)
-- getLegalMoveTo'' lm forbidden (x0, x1, x2, x3) =
--   let cf = complement forbidden
--       y0 = getClosedNeighborhood (x0 .|. x1 .|. x2 .|. x3) .&. (lm !! 0) .&. cf
--       y1 = getClosedNeighborhood (x0 .|. x1 .|. x2 .|. x3) .&. (lm !! 1) .&. cf
--       y2 = getClosedNeighborhood (x1 .|. x2 .|. x3) .&. (lm !! 2) .&. cf
--       y3 = getClosedNeighborhood x3 .&. (lm !! 3) .&. cf
--       z = getClosedNeighborhood x2 .&. (lm !! 3) .&. cf
--    in ((y0, y1, y2, y3), z)
-- --
-- getLegalMoveTo (Just Artemis) mf pm lm =
--   let mfBB = singletonBB mf
--       (xs, a) = getLegalMoveTo'' lm (pm ! 6) (mfBB .&. (lm !! 0), mfBB .&. (lm !! 1), mfBB .&. (lm !! 2), mfBB .&. (lm !! 3))
--       ((y0, y1, y2, y3), b) = getLegalMoveTo'' lm (pm ! 6) xs
--    in bbToList $ y0 .|. y1 .|. y2 .|. y3 .|. a .|. b
-- =========================================================================================
--
-- [Minotaur]
-- A token’s move can optionally enter the space of an opponent’s token,
-- but only if the token can be pushed back to an unoccupied space, and only as long as
-- the token would be able to move to the opponent’s space if the opponent token were not there.
-- The unoccupied space where the opponent’s token is pushed can be at any level less than 4.
getLegalMoveTo (Just Minotaur) mf pm lm =
  let candidates = getLegalMoveTo' mf lm `andNotBB` (if mf `elemBB` (pm ! 4) then pm ! 4 else pm ! 5)
      emptySpace = (lm !! 7) `andNotBB` (pm ! 6)
      isValidMoveTo mt = mt `elemBB` emptySpace || getPointSymmetricIndex mt mf `elemBB` emptySpace -- works only if mf and mt are adjacent
   in filter isValidMoveTo (bbToList candidates)
--
-- [Apollo]
-- A token’s move can optionally swap places with an adjacent opponent token,
-- as long as the token would be able to move to the opponent’s space if the
-- opponent token were not there; otherwise, the move must be to an unoccupied space as usual.
getLegalMoveTo (Just Apollo) mf pm lm = bbToList $ getLegalMoveTo' mf lm `andNotBB` (if mf `elemBB` (pm ! 4) then pm ! 4 else pm ! 5)
--
-- Others.
getLegalMoveTo _ mf pm lm = bbToList $ getLegalMoveTo' mf lm `andNotBB` (pm ! 6)

--------------------------------------------------------------------------------
-- Push to
--------------------------------------------------------------------------------
getLegalPushTo :: Maybe Card -> Players -> Index -> Index -> Maybe (WorkerId, Index)
getLegalPushTo (Just Apollo) [_, p] mf mt | mt `elem` p = Just (if p !! 0 == mt then 0 else 1, mf)
getLegalPushTo (Just Apollo) [p, _] mf mt | mt `elem` p = Just (if p !! 0 == mt then 0 else 1, mf)
getLegalPushTo (Just Minotaur) [_, p] mf mt | mt `elem` p = Just (if p !! 0 == mt then 0 else 1, getPointSymmetricIndex mt mf)
getLegalPushTo (Just Minotaur) [p, _] mf mt | mt `elem` p = Just (if p !! 0 == mt then 0 else 1, getPointSymmetricIndex mt mf)
getLegalPushTo _ _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Build at
--------------------------------------------------------------------------------

-- taken from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | y : xs' <- tails xs, ys <- combinations (n -1) xs']

getLegalBuildAt :: Maybe Card -> Levels -> BitBoard -> Index -> Index -> [[(Index, Level, Level)]]
--
-- [Atlas]
-- The build phase can build a space currently at level 0, 1, 2 to make it level 4,
-- instead of building to exactly one more than the space’s current level.
getLegalBuildAt (Just Atlas) lv emptySpace _ mt =
  let hs l = if l == 3 then [4] else [l + 1, 4]
   in [[(bl, lv ! bl, h)] | bl <- bbToList $ getNeighborhood mt .&. emptySpace, h <- hs (lv ! bl)]
--
-- [Demeter]
-- The moved token can optionally build a second time, but not on the same space
-- as the first build within a turn.
getLegalBuildAt (Just Demeter) lv emptySpace _ mt =
  let cand = bbToList $ getNeighborhood mt .&. emptySpace
      comb = [[x] | x <- cand] ++ combinations 2 cand
   in [[(x, lv ! x, (lv ! x) + 1) | x <- xs] | xs <- comb]
--
-- [Hephastus]
-- The moved token can optionally build a second time, but only on the same space
-- as the first build within a turn, and only if the second build does not reach level 4.
getLegalBuildAt (Just Hephastus) lv emptySpace _ mt =
  let hs l = if l <= 1 then [l + 1, l + 2] else [l + 1]
   in [[(bl, lv ! bl, h)] | bl <- bbToList $ getNeighborhood mt .&. emptySpace, h <- hs (lv ! bl)]
--
-- [Prometheus]
-- A token can optionally build before moving, but then the move is constrained to
-- the same level or lower (i.e., the level of the token’s new space can be no larger
-- than the level of the token’s old space).
getLegalBuildAt (Just Prometheus) lv emptySpace mf mt
  | (lv ! mf) >= (lv ! mt) =
    let secondBuild = bbToList $ getNeighborhood mt .&. emptySpace
        forbidden = if (lv ! mf) == (lv ! mt) then complement (1 `shift` mt) else -1
        firstBuild = bbToList $ getNeighborhood mf .&. emptySpace .&. forbidden
     in do
          x <- secondBuild
          let lvx = lv ! x
          doubleBuild <- [False, True]
          if not doubleBuild
            then return [(x, lvx, lvx + 1)]
            else do
              y <- firstBuild
              if x == y
                then do
                  guard $ lvx <= 2
                  return [(x, lvx, lvx + 2)]
                else do
                  let lvy = lv ! y
                  return [(x, lvx, lvx + 1), (y, lvy, lvy + 1)]

-- [ if x == y
--     then [(x, lv ! x, (lv ! x) + 2)]
--     else [(x, lv ! x, (lv ! x) + 1), (y, lv ! y, (lv ! y) + 1)]
--   | x <- secondBuild,
--     y <- firstBuild,
--     x /= y || lv ! x <= 2
-- ]
--   ++ (getLegalBuildAt Nothing lv emptySpace mf mt)
--
-- Others.
getLegalBuildAt _ lv emptySpace _ mt = [[(bl, lv ! bl, (lv ! bl) + 1)] | bl <- bbToList $ getNeighborhood mt .&. emptySpace]

--------------------------------------------------------------------------------
-- All legal moves
--------------------------------------------------------------------------------

getLegalMoves' :: Bool -> GameState -> [GameMove]
getLegalMoves'
  effectiveOnly
  GameState
    { cards = cs,
      players = ps,
      playerMap = pm,
      levels = lv,
      levelMap = lm
    } = getLegalMoves effectiveOnly cs ps pm lv lm

getLegalMoves :: Bool -> Cards -> Players -> PlayerMap -> Levels -> LevelMap -> [GameMove]
getLegalMoves effectiveOnly [c1, c2] pl pm lv lm =
  let allMoves = do
        wk <- [0, 1]
        let mf = pl !! 0 !! wk -- move from

        -- move to
        mt <- getLegalMoveTo c1 mf pm lm
        let applyDoubleMove = if mt `elemBB` getLegalMoveTo' mf lm then id else setDoubleMove

        -- push to
        let pushInfo = getLegalPushTo c1 pl mf mt
        let applyPushTo = maybe id (uncurry setOpponentMove) pushInfo

        -- update player map
        let moveDiff = listToBB [mf, mt]
        let pushDiff = maybe 0 (\(_, pt) -> listToBB [mt, pt]) pushInfo
        let pm' = foldl' (flip (Map.adjust (xor moveDiff))) pm [wk, 4, 6]
        let pm'' = maybe id (\(wid, _) mm -> foldl' (flip (Map.adjust (xor pushDiff))) mm [2 + wid, 5, 6]) pushInfo pm'

        -- check point 1
        let moveSofar = (applyPushTo . applyDoubleMove . setMoveTo mt . setMoveFrom mf . setWorkerId wk) createGameMove

        -- check winning
        -- Note: it's possible for Artemis to win by moving like 1->2->3 or 3->2->3
        if isWinningMove c1 mf mt lm
          then -- if there is a winning move, do not build (thus, do not generate all moves for Artemis)
            return $ setWin moveSofar
          else do
            -- build at
            let emptySpace = ((lm !! 7) `andNotBB` (pm'' ! 6)) .|. singletonBB mt
            bls <- getLegalBuildAt c1 lv emptySpace mf mt

            -- check next levels
            let lv' = makeNextLevels lv bls
            let lm' = makeNextLevelMap lm bls

            -- check point 2
            let moveSofar' = (setMoveToLevel (lv' ! mt) . setBuildAt bls) moveSofar

            -- evaluation
            if hasWinningMove c2 1 pm'' lm'
              then do
                let moveSofar'' = setLose moveSofar'
                guard $ not effectiveOnly
                return moveSofar''
              else do
                -- final result
                let buildTo = listToBB [x | (x, _, _) <- bls]
                let applyBlocking = if isBlocking buildTo pm lm then setBlocking else id
                let applyStepping = if isStepping buildTo pm'' lm' then setStepping else id
                return $ (applyBlocking . applyStepping) moveSofar'
   in if effectiveOnly
        then case find getWin allMoves of
          Just m -> [m] -- one winning move is enough
          Nothing -> sort allMoves
        else sort allMoves
getLegalMoves _ _ _ _ _ _ = undefined

--------------------------------------------------------------------------------
-- Checking Winning Moves
--------------------------------------------------------------------------------

isWinningMove :: Maybe Card -> Index -> Index -> LevelMap -> Bool
-- Artemis' move lv 3 -> lv 2 -> lv 3
isWinningMove (Just Artemis) mf mt lm
  | mt `elemBB` (lm !! 3) && mf `elemBB` (lm !! 3) = getNeighborhood mf .&. getNeighborhood mt .&. (lm !! 2) /= 0
isWinningMove (Just Pan) mf mt lm
  | mf `elemBB` (lm !! 2) = mt `elemBB` ((lm !! 3) .|. (lm !! 0))
  | mf `elemBB` (lm !! 3) = mt `elemBB` ((lm !! 0) .|. (lm !! 1))
isWinningMove _ mf mt lm = mt `elemBB` (lm !! 3) && mf `elemBB` (lm !! 6)

hasWinningMove :: Maybe Card -> Int -> PlayerMap -> LevelMap -> Bool
-- Pan: ((N[P4 & L2] & (L0 & L3)) | (N[P4 & L3] & L5)) & ~P6
hasWinningMove (Just Pan) p pm lm =
  let fromLv2 = getClosedNeighborhood ((pm ! (4 + p)) .&. (lm !! 2)) .&. ((lm !! 0) .|. (lm !! 3))
      fromLv3 = getClosedNeighborhood ((pm ! (4 + p)) .&. (lm !! 3)) .&. (lm !! 5)
   in (fromLv2 .|. fromLv3) `andNotBB` (pm ! 6) /= 0
-- Artemis: N[L3 & ~P6] & L2 & (P4 | (N[P4 & ~L0] & ~P6))
hasWinningMove (Just Artemis) p pm lm =
  let x = (pm ! (4 + p)) `andNotBB` (lm !! 0) -- Artemis workers at Lv 1, 2, or 3
      a = getClosedNeighborhood ((lm !! 3) `andNotBB` (pm ! 6)) .&. (lm !! 2) -- Lv2 spaces next to empty Lv3
      b = getClosedNeighborhood x `andNotBB` (pm ! 6) -- Artemis workers' empty neighbors
   in a .&. (x .|. b) /= 0
-- Minotaur
hasWinningMove (Just Minotaur) p pm lm =
  let x = (pm ! (4 + p)) .&. (lm !! 2)
      y = getClosedNeighborhood x .&. (lm !! 3) .&. (pm ! (5 - p))
      q = getPushBB x y
   in hasWinningMove Nothing p pm lm || q .&. (lm !! 7) `andNotBB` (pm ! 6) /= 0
-- Others: N[P4 & L2] & L3 & ~P6
hasWinningMove _ p pm lm =
  getClosedNeighborhood ((pm ! (4 + p)) .&. (lm !! 2)) .&. (lm !! 3) `andNotBB` (pm ! 6) /= 0

isBlocking :: BitBoard -> PlayerMap -> LevelMap -> Bool
isBlocking buildTo pmBefore lm =
  -- lm before
  let fromLv0 = getClosedNeighborhood ((pmBefore ! 5) .&. (lm !! 0)) .&. (lm !! 1)
      fromLv1 = getClosedNeighborhood ((pmBefore ! 5) .&. (lm !! 1)) .&. (lm !! 2)
   in buildTo .&. (fromLv0 .|. fromLv1) /= 0

isStepping :: BitBoard -> PlayerMap -> LevelMap -> Bool
isStepping buildTo pmAfter lm =
  -- lm after
  let fromLv0 = getClosedNeighborhood ((pmAfter ! 4) .&. (lm !! 0)) .&. (lm !! 1)
      fromLv1 = getClosedNeighborhood ((pmAfter ! 4) .&. (lm !! 1)) .&. (lm !! 2)
   in buildTo .&. (fromLv0 .|. fromLv1) /= 0

--------------------------------------------------------------------------------
-- Making Moves
--------------------------------------------------------------------------------

makeNextPlayers :: Players -> GameMove -> Players
makeNextPlayers [p1, p2] mv =
  let (wk, mt) = (getWorkerId mv, getMoveTo mv)
      (owk, omt) = getOpponentMove mv
      p1' = [if wk == i then mt else p1 !! i | i <- [0, 1]]
      p2' = [if owk == i then omt else p2 !! i | i <- [0, 1]]
   in [p2', p1']
makeNextPlayers _ _ = undefined

makeNextLevels :: Levels -> [(Index, Level, Level)] -> Levels
makeNextLevels = foldl' (\xlv (bl, _, nextLevel) -> Map.insert bl nextLevel xlv)

makeNextLevelMap :: LevelMap -> [(Index, Level, Level)] -> LevelMap
-- Note: These definitions are tedious yet more performant.
makeNextLevelMap lm [] = lm
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 1)] =
  let bb1 = singletonBB bl1
   in [x0 `xor` bb1, x1 `xor` bb1, x2, x3, x4, x5, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 2)] =
  let bb1 = singletonBB bl1
   in [x0 `xor` bb1, x1, x2 `xor` bb1, x3, x4, x5 `xor` bb1, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 4)] =
  let bb1 = singletonBB bl1
   in [x0 `xor` bb1, x1, x2, x3, x4 `xor` bb1, x5 `xor` bb1, x6 `xor` bb1, x7 `xor` bb1]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 2)] =
  let bb1 = singletonBB bl1
   in [x0, x1 `xor` bb1, x2 `xor` bb1, x3, x4, x5 `xor` bb1, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 3)] =
  let bb1 = singletonBB bl1
   in [x0, x1 `xor` bb1, x2, x3 `xor` bb1, x4, x5 `xor` bb1, x6 `xor` bb1, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 4)] =
  let bb1 = singletonBB bl1
   in [x0, x1 `xor` bb1, x2, x3, x4 `xor` bb1, x5 `xor` bb1, x6 `xor` bb1, x7 `xor` bb1]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 2, 3)] =
  let bb1 = singletonBB bl1
   in [x0, x1, x2 `xor` bb1, x3 `xor` bb1, x4, x5, x6 `xor` bb1, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 2, 4)] =
  let bb1 = singletonBB bl1
   in [x0, x1, x2 `xor` bb1, x3, x4 `xor` bb1, x5, x6 `xor` bb1, x7 `xor` bb1]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 3, 4)] =
  let bb1 = singletonBB bl1
   in [x0, x1, x2, x3 `xor` bb1, x4 `xor` bb1, x5, x6, x7 `xor` bb1]
-- double build
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 1), (bl2, 0, 1)] =
  let bb = singletonBB bl1 `xor` singletonBB bl2
   in [x0 `xor` bb, x1 `xor` bb, x2, x3, x4, x5, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 1), (bl2, 1, 2)] =
  let bb1 = singletonBB bl1
      bb2 = singletonBB bl2
      bb = bb1 `xor` bb2
   in [x0 `xor` bb1, x1 `xor` bb, x2 `xor` bb2, x3, x4, x5 `xor` bb2, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 1), (bl2, 2, 3)] =
  let bb1 = singletonBB bl1
      bb2 = singletonBB bl2
   in [x0 `xor` bb1, x1 `xor` bb1, x2 `xor` bb2, x3 `xor` bb2, x4, x5, x6 `xor` bb2, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 0, 1), (bl2, 3, 4)] =
  let bb1 = singletonBB bl1
      bb2 = singletonBB bl2
   in [x0 `xor` bb1, x1 `xor` bb1, x2, x3 `xor` bb2, x4 `xor` bb2, x5, x6, x7 `xor` bb2]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 2), (bl2, 0, 1)] =
  let bb1 = singletonBB bl2
      bb2 = singletonBB bl1
      bb = bb1 `xor` bb2
   in [x0 `xor` bb1, x1 `xor` bb, x2 `xor` bb2, x3, x4, x5 `xor` bb2, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 2), (bl2, 1, 2)] =
  let bb = singletonBB bl1 `xor` singletonBB bl2
   in [x0, x1 `xor` bb, x2 `xor` bb, x3, x4, x5 `xor` bb, x6, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 2), (bl2, 2, 3)] =
  let bb1 = singletonBB bl1
      bb2 = singletonBB bl2
      bb = bb1 `xor` bb2
   in [x0, x1 `xor` bb1, x2 `xor` bb, x3 `xor` bb2, x4, x5 `xor` bb1, x6 `xor` bb2, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 1, 2), (bl2, 3, 4)] =
  let bb1 = singletonBB bl1
      bb2 = singletonBB bl2
   in [x0, x1 `xor` bb1, x2 `xor` bb1, x3 `xor` bb2, x4 `xor` bb2, x5 `xor` bb1, x6, x7 `xor` bb2]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 2, 3), (bl2, 0, 1)] =
  let bb1 = singletonBB bl2
      bb2 = singletonBB bl1
   in [x0 `xor` bb1, x1 `xor` bb1, x2 `xor` bb2, x3 `xor` bb2, x4, x5, x6 `xor` bb2, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 2, 3), (bl2, 1, 2)] =
  let bb1 = singletonBB bl2
      bb2 = singletonBB bl1
      bb = bb1 `xor` bb2
   in [x0, x1 `xor` bb1, x2 `xor` bb, x3 `xor` bb2, x4, x5 `xor` bb1, x6 `xor` bb2, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 2, 3), (bl2, 2, 3)] =
  let bb = singletonBB bl1 `xor` singletonBB bl2
   in [x0, x1, x2 `xor` bb, x3 `xor` bb, x4, x5, x6 `xor` bb, x7]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 2, 3), (bl2, 3, 4)] =
  let bb1 = singletonBB bl1
      bb2 = singletonBB bl2
      bb = bb1 `xor` bb2
   in [x0, x1, x2 `xor` bb1, x3 `xor` bb, x4 `xor` bb2, x5, x6 `xor` bb1, x7 `xor` bb2]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 3, 4), (bl2, 0, 1)] =
  let bb1 = singletonBB bl2
      bb2 = singletonBB bl1
   in [x0 `xor` bb1, x1 `xor` bb1, x2, x3 `xor` bb2, x4 `xor` bb2, x5, x6, x7 `xor` bb2]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 3, 4), (bl2, 1, 2)] =
  let bb1 = singletonBB bl2
      bb2 = singletonBB bl1
   in [x0, x1 `xor` bb1, x2 `xor` bb1, x3 `xor` bb2, x4 `xor` bb2, x5 `xor` bb1, x6, x7 `xor` bb2]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 3, 4), (bl2, 2, 3)] =
  let bb1 = singletonBB bl2
      bb2 = singletonBB bl1
      bb = bb1 `xor` bb2
   in [x0, x1, x2 `xor` bb1, x3 `xor` bb, x4 `xor` bb2, x5, x6 `xor` bb1, x7 `xor` bb2]
makeNextLevelMap [x0, x1, x2, x3, x4, x5, x6, x7] [(bl1, 3, 4), (bl2, 3, 4)] =
  let bb = singletonBB bl1 `xor` singletonBB bl2
   in [x0, x1, x2, x3 `xor` bb, x4 `xor` bb, x5, x6, x7 `xor` bb]
makeNextLevelMap _ _ = undefined

-- makeNextLevelMap lm bls =
--   let (v0, v1, v2, v3, v4) =
--         foldl'
--           ( \(x0, x1, x2, x3, x4) (bl, prevLv, nextLv) ->
--               let bb = singletonBB bl
--                in --  in case prevLv of
--                   -- 0 -> case nextLv of
--                   --   1 -> (x0 `xor` bb, x1 `xor` bb, x2, x3, x4)
--                   --   2 -> (x0 `xor` bb, x1, x2 `xor` bb, x3, x4)
--                   --   3 -> (x0 `xor` bb, x1, x2, x3 `xor` bb, x4)
--                   --   _ -> (x0 `xor` bb, x1, x2, x3, x4 `xor` bb)
--                   -- 1 -> case nextLv of
--                   --   2 -> (x0, x1 `xor` bb, x2 `xor` bb, x3, x4)
--                   --   3 -> (x0, x1 `xor` bb, x2, x3 `xor` bb, x4)
--                   --   _ -> (x0, x1 `xor` bb, x2, x3, x4 `xor` bb)
--                   -- 2 -> case nextLv of
--                   --   3 -> (x0, x1, x2 `xor` bb, x3 `xor` bb, x4)
--                   --   _ -> (x0, x1, x2 `xor` bb, x3, x4 `xor` bb)
--                   -- _ -> (x0, x1, x2, x3 `xor` bb, x4 `xor` bb)
--                   case (prevLv, nextLv) of
--                     (0, 1) -> (x0 `xor` bb, x1 `xor` bb, x2, x3, x4)
--                     (0, 2) -> (x0 `xor` bb, x1, x2 `xor` bb, x3, x4)
--                     (0, 3) -> (x0 `xor` bb, x1, x2, x3 `xor` bb, x4)
--                     (0, _) -> (x0 `xor` bb, x1, x2, x3, x4 `xor` bb)
--                     (1, 2) -> (x0, x1 `xor` bb, x2 `xor` bb, x3, x4)
--                     (1, 3) -> (x0, x1 `xor` bb, x2, x3 `xor` bb, x4)
--                     (1, _) -> (x0, x1 `xor` bb, x2, x3, x4 `xor` bb)
--                     (2, 3) -> (x0, x1, x2 `xor` bb, x3 `xor` bb, x4)
--                     (2, _) -> (x0, x1, x2 `xor` bb, x3, x4 `xor` bb)
--                     _ -> (x0, x1, x2, x3 `xor` bb, x4 `xor` bb) -- 3 -> 4
--           )
--           (lm !! 0, lm !! 1, lm !! 2, lm !! 3, lm !! 4)
--           bls
--       v5 = v0 + v1 -- level 0 or 1
--       v6 = v5 + v2 -- level 0, 1 or 2
--       v7 = v6 + v3 -- level 0, 1, 2 or 3
--    in [v0, v1, v2, v3, v4, v5, v6, v7]

makeMove :: GameState -> GameMove -> GameState
makeMove
  GameState
    { cards = [c1, c2],
      players = pl,
      levels = lv,
      turn = t,
      levelMap = lm
    }
  mv =
    -- assume: m `elem` mv
    let t' = t + 1
        cs' = [c2, c1]
        pl' = makeNextPlayers pl mv
        pm' = createPlayerMap pl'
        builds = getBuildAt mv
        lv' = makeNextLevels lv builds
        lm' = makeNextLevelMap lm builds
        mv' = getLegalMoves True cs' pl' pm' lv' lm'
     in GameState {cards = cs', players = pl', playerMap = pm', levels = lv', turn = t', levelMap = lm', legalMoves = mv'}
makeMove _ _ = undefined
