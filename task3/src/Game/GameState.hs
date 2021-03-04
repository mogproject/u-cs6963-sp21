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
    legalMoves,
    Index,
    Cards,
    Players,
    Levels,
    LevelMap,
    AdjList,
    getLegalMoves',
    getLegalMoveTo,
    getLegalBuildAt,
  )
where

import Control.Monad (guard)
import Data.Bits (complement, shift, xor, (.&.), (.|.))
import Data.Board (Board (Board), Level, Player (Player))
import qualified Data.Board
import qualified Data.BoardWithoutCard
import Data.Card (Card (Apollo, Artemis, Atlas, Demeter, Hephastus, Minotaur, Pan, Prometheus))
import Data.List (find, sort, tails)
import Data.Map (Map, (!))
import qualified Data.Map
import Game.BitBoard
import Game.GameMove

type WorkerId = Int -- 0 or 1

type Cards = [Maybe Card]

type Players = [[Index]]

type Turn = Int

type Levels = Map Index Level

type LevelMap = Map Level BitBoard

type AdjList = Map Index BitBoard -- adjacency list from each index

-- Internal representation of game states.
data GameState = GameState
  { cards :: Cards, -- Cards (player to move, player to wait)
    players :: Players, -- Players (player to move, player to wait)
    levels :: Levels, -- Levels
    turn :: Turn,
    levelMap :: LevelMap, -- level [0..4] -> bitmap
    moveAdjacency :: AdjList, -- move adjacency list: index -> bitboard
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
      lv = Data.Map.fromList [(posToIndex (r, c), sp !! (r -1) !! (c -1)) | r <- [1 .. 5], c <- [1 .. 5]]
      lm = createLevelMap lv
      adj = createMoveAdjacencyList lv lm
      mv = getLegalMoves True cs pl lv lm adj
   in GameState {cards = cs, players = pl, levels = lv, turn = t, levelMap = lm, moveAdjacency = adj, legalMoves = mv}

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

--------------------------------------------------------------------------------
-- Moves
--------------------------------------------------------------------------------

createLevelMap :: Levels -> LevelMap
createLevelMap lv =
  let m = Data.Map.fromListWith (.|.) [(v, singletonBB k) | (k, v) <- Data.Map.toList lv]
   in Data.Map.union m (Data.Map.fromList [(l, 0) | l <- [0 .. 4]])

createMoveAdjacencyList :: Levels -> LevelMap -> AdjList
createMoveAdjacencyList lv lm =
  let f i = getNeighborhood i .&. sum [lm ! h | h <- [0 .. (min 3 ((lv ! i) + 1))]]
   in Data.Map.fromList [(i, if lv ! i == 4 then 0 else f i) | i <- validIndices]

--------------------------------------------------------------------------------
-- Move to
--------------------------------------------------------------------------------
getLegalMoveTo :: Maybe Card -> Index -> LevelMap -> BitBoard -> BitBoard -> BitBoard -> AdjList -> [Index]
--
-- [Artemis]
-- The moved token can optionally move a second time (i.e., the same token),
-- as long as the first move doesn’t win, and as long as the second move doesn’t return
-- to the original space.
getLegalMoveTo (Just Artemis) mf lm _ allWorkers _ adj =
  let firstMove = (adj ! mf) `andNotBB` allWorkers
      secondMoveFrom = (if mf `elemBB` (lm ! 2) then (\x -> x `andNotBB` (lm ! 3)) else id) firstMove
   in bbToList $ foldl (\z x -> z .|. adj ! x) firstMove (bbToList secondMoveFrom) `andNotBB` allWorkers
--
-- [Minotaur]
-- A token’s move can optionally enter the space of an opponent’s token,
-- but only if the token can be pushed back to an unoccupied space, and only as long as
-- the token would be able to move to the opponent’s space if the opponent token were not there.
-- The unoccupied space where the opponent’s token is pushed can be at any level less than 4.
getLegalMoveTo (Just Minotaur) mf _ friend _ emptySpace adj =
  let candidates = (adj ! mf) `andNotBB` friend
      isValidMoveTo mt = getPointSymmetricIndex mt mf `elemBB` emptySpace -- works only if mf and mt are adjacent
   in filter (\x -> x `elemBB` emptySpace || isValidMoveTo x) (bbToList candidates)
--
-- [Apollo]
-- A token’s move can optionally swap places with an adjacent opponent token,
-- as long as the token would be able to move to the opponent’s space if the
-- opponent token were not there; otherwise, the move must be to an unoccupied space as usual.
getLegalMoveTo (Just Apollo) mf _ friend _ _ adj = bbToList $ (adj ! mf) `andNotBB` friend
--
-- Others.
getLegalMoveTo _ mf _ _ allWorkers _ adj = bbToList $ (adj ! mf) `andNotBB` allWorkers

--------------------------------------------------------------------------------
-- Push to
--------------------------------------------------------------------------------
getLegalPushTo :: Maybe Card -> Players -> Index -> Index -> Maybe (WorkerId, Index)
getLegalPushTo (Just Apollo) [_, p] mf mt | mt `elem` p = Just (if p !! 0 == mt then 0 else 1, mf)
getLegalPushTo (Just Minotaur) [_, p] mf mt | mt `elem` p = Just (if p !! 0 == mt then 0 else 1, getPointSymmetricIndex mt mf)
getLegalPushTo _ _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Build at
--------------------------------------------------------------------------------

-- taken from https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | y : xs' <- tails xs, ys <- combinations (n -1) xs']

getLegalBuildAt :: Maybe Card -> Levels -> BitBoard -> Index -> Index -> [[(Index, Level)]]
--
-- [Atlas]
-- The build phase can build a space currently at level 0, 1, 2 to make it level 4,
-- instead of building to exactly one more than the space’s current level.
getLegalBuildAt (Just Atlas) lv emptySpace _ mt =
  let hs l = if l == 3 then [4] else [l + 1, 4]
   in [[(bl, h)] | bl <- bbToList $ getNeighborhood mt .&. emptySpace, h <- hs (lv ! bl)]
--
-- [Demeter]
-- The moved token can optionally build a second time, but not on the same space
-- as the first build within a turn.
getLegalBuildAt (Just Demeter) lv emptySpace _ mt =
  let cand = bbToList $ getNeighborhood mt .&. emptySpace
      comb = [[x] | x <- cand] ++ combinations 2 cand
   in [[(x, (lv ! x) + 1) | x <- xs] | xs <- comb]
--
-- [Hephastus]
-- The moved token can optionally build a second time, but only on the same space
-- as the first build within a turn, and only if the second build does not reach level 4.
getLegalBuildAt (Just Hephastus) lv emptySpace _ mt =
  let hs l = if l <= 1 then [l + 1, l + 2] else [l + 1]
   in [[(bl, h)] | bl <- bbToList $ getNeighborhood mt .&. emptySpace, h <- hs (lv ! bl)]
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
     in getLegalBuildAt Nothing lv emptySpace mf mt
          ++ [ if x == y then [(x, (lv ! x) + 2)] else [(x, (lv ! x) + 1), (y, (lv ! y) + 1)]
               | x <- secondBuild,
                 y <- firstBuild,
                 x /= y || lv ! x <= 2
             ]
--
-- Others.
getLegalBuildAt _ lv emptySpace _ mt = [[(bl, (lv ! bl) + 1)] | bl <- bbToList $ getNeighborhood mt .&. emptySpace]

--------------------------------------------------------------------------------
-- All legal moves
--------------------------------------------------------------------------------

getLegalMoves' :: Bool -> GameState -> [GameMove]
getLegalMoves'
  effectiveOnly
  GameState
    { cards = cs,
      players = ps,
      levels = lv,
      levelMap = lm,
      moveAdjacency = adj
    } = getLegalMoves effectiveOnly cs ps lv lm adj

getLegalMoves :: Bool -> Cards -> Players -> Levels -> LevelMap -> AdjList -> [GameMove]
getLegalMoves effectiveOnly [c1, c2] pl lv lm adj =
  let allMoves = do
        wk <- [0, 1]
        let mf = pl !! 0 !! wk -- move from

        -- bitboards
        let opponentWorkers = (sum . map singletonBB) (pl !! 1)
        let friendWorker = singletonBB (pl !! 0 !! (1 - wk))
        let otherWorkers = opponentWorkers .|. friendWorker
        let allWorkers = otherWorkers .|. singletonBB (pl !! 0 !! wk)
        let emptySpace = globalMask `andNotBB` ((lm ! 4) .|. otherWorkers)

        -- move to
        mt <- getLegalMoveTo c1 mf lm friendWorker allWorkers emptySpace adj
        let applyDoubleMove = if mt `elemBB` (adj ! mf) then id else setDoubleMove

        -- push to
        let pushInfo = getLegalPushTo c1 pl mf mt
        let applyPushTo = maybe id (uncurry setOpponentMove) pushInfo

        -- check point 1
        let moveSofar = (applyPushTo . applyDoubleMove . setMoveToLevel (lv ! mt) . setMoveTo mt . setMoveFrom mf . setWorkerId wk) createGameMove
        let emptySofar = emptySpace .&. complement (maybe 0 (\(_, pushTo) -> 1 `shift` pushTo) pushInfo)

        -- check winning
        -- Note: it's possible for Artemis to win by moving like 1->2->3 or 3->2->3
        if isWinningMove c1 mf mt lv lm
          then -- if there is a winning move, do not build (thus, do not generate all moves for Artemis)
            return $ setWin moveSofar
          else do
            -- build at
            bls <- getLegalBuildAt c1 lv emptySofar mf mt

            -- check point 2
            let moveSofar' = setBuildAt bls moveSofar

            -- check next levels
            let (lv', lm') =
                  foldl
                    ( \(xlv, xlm) (bl, nextLevel) ->
                        let prevLevel = xlv ! bl
                            f = Data.Map.adjust (xor (singletonBB bl)) -- toggle bl bit
                            xlv' = Data.Map.insert bl nextLevel xlv
                            xlm' = f nextLevel $ f prevLevel xlm
                         in (xlv', xlm')
                    )
                    (lv, lm)
                    bls

            -- evaluation
            if isLosingMove c2 (pl !! 1) lv' lm'
              then do
                let moveSofar'' = setLose moveSofar'
                guard $ not effectiveOnly
                return moveSofar''
              else -- TODO: Implement
                return moveSofar'
   in if effectiveOnly
        then case find getWin allMoves of
          Just m -> [m] -- one winning move is enough
          Nothing -> sort allMoves
        else sort allMoves
getLegalMoves _ _ _ _ _ _ = undefined

isWinningMove :: Maybe Card -> Index -> Index -> Levels -> LevelMap -> Bool
-- Artemis' move lv 3 -> lv 2 -> lv 3
isWinningMove (Just Artemis) mf mt lv lm | (lv ! mt) == 3 && (lv ! mf) == 3 = getNeighborhood mf .&. getNeighborhood mt .&. (lm ! 2) /= 0
isWinningMove (Just Pan) mf mt lv lm = (lv ! mt) + 2 <= (lv ! mf) || isWinningMove Nothing mf mt lv lm
isWinningMove _ mf mt lv _ = (lv ! mt) == 3 && (lv ! mf) < 3

isLosingMove :: Maybe Card -> [Index] -> Levels -> LevelMap -> Bool
isLosingMove _ pl _ lm = getClosedNeighborhood (listToBB pl .&. lm ! 2) .&. lm ! 3 /= 0

makeMove :: GameState -> GameMove -> GameState
makeMove
  GameState
    { cards = [c1, c2],
      players = [p1, p2],
      levels = lv,
      turn = t,
      levelMap = lm,
      moveAdjacency = adj
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

        (lv', lm', adj') =
          foldl
            ( \(xlv, xlm, xadj) (bl, nextLevel) ->
                let prevLevel = xlv ! bl
                    f = Data.Map.adjust (xor (singletonBB bl)) -- toggle bl bit
                    f' = Data.Map.adjust (\x -> x `andNotBB` singletonBB bl) -- remove bl bit
                    g lo hi = sum [xlm ! h | h <- [lo .. hi]] -- merge level range

                    -- levels
                    xlv' = Data.Map.insert bl nextLevel xlv
                    -- level map
                    xlm' = f nextLevel $ f prevLevel xlm

                    -- update adjacency lists
                    addArc m =
                      if prevLevel <= 1 -- add arc: bl -> high N(bl)
                        then Data.Map.adjust (.|. (getNeighborhood bl .&. g (prevLevel + 2) (min 3 (nextLevel + 1)))) bl m
                        else m
                    removeArc m =
                      if nextLevel >= 2 -- remove arc: low N(bl) -> bl
                        then foldl (flip f) m (bbToList (getNeighborhood bl .&. g (max 0 (prevLevel - 1)) (nextLevel - 2)))
                        else m

                    xadj'
                      | prevLevel == nextLevel = xadj
                      | nextLevel == 4 = foldl (flip f') (Data.Map.insert bl 0 xadj) (bbToList (getNeighborhood bl `andNotBB` (xlm ! 4))) -- capped
                      | otherwise = (removeArc . addArc) xadj
                 in (xlv', xlm', xadj')
            )
            (lv, lm, adj)
            builds

        mv' = getLegalMoves True cs' pl' lv' lm' adj'
     in GameState {cards = cs', players = pl', levels = lv', turn = t', levelMap = lm', moveAdjacency = adj', legalMoves = mv'}
makeMove _ _ = undefined
