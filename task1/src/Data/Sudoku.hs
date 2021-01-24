module Data.Sudoku
  ( Sudoku,
    Pos,
    ExtendedPos,
    readSudoku,
    emptySudoku,
    insert,
    getEmptyPositions,
    getAvailableNumbers,
    getRegionId,
  )
where

import Control.Monad (foldM)
import Data.BitSet (BitSet, complement, delete, empty, intersection, notMember, toList)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Map (Map, (!))
import qualified Data.Map
import Text.Read (readEither)

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------

-- | Height of each region.
type RegionHeight = Int

-- | Width of each region.
type RegionWidth = Int

-- | Row of the board (0-indexed).
type RowId = Int

-- | Column of the board (0-indexed).
type ColumnId = Int

-- | Region ID (0-indexed).
type RegionId = Int

-- | Position of a grid on the board.
type Pos = (RowId, ColumnId)

-- | Position plus region ID.
type ExtendedPos = (RowId, ColumnId, RegionId)

-- | Internal representation of grid values (0-indexed).
type Entry = Int

-- | Board information
-- | - board representation; map from positions to entries
-- | - row constraints; available numbers for each row
-- | - column constraints; available numbers for each column
-- | - region constraints; available numbers for each region
type BoardInfo = (Map Pos Entry, Map RowId BitSet, Map ColumnId BitSet, Map RegionId BitSet)

-- | Representation of a sudoku board and pre-computed availability
data Sudoku
  = Sudoku
      RegionHeight
      -- ^ n: Height of each region
      RegionWidth
      -- ^ m: Width of each region
      BoardInfo
      -- ^ Board information
  deriving (Ord, Eq)

-- | Creates an empty sudoku board.
emptySudoku :: RegionHeight -> RegionWidth -> Sudoku
emptySudoku n m = Sudoku nn mm (Data.Map.empty, initConstraints, initConstraints, initConstraints)
  where
    nn = max 1 n
    mm = max 1 m
    -- initially, all numbers are available
    initConstraints = Data.Map.fromList [(i, complement (Data.BitSet.empty $ nn * mm)) | i <- [0 .. nn * mm - 1]]

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

-- | Converts a Sudoku instance to String.
instance Show Sudoku where
  show (Sudoku n m (board, _, _, _)) = unlines $ dimensions : [showRow r | r <- [0 .. n * m -1]]
    where
      dimensions = unwords $ map show [n, m]
      showEntry pos = maybe "." (\x -> show (x + 1)) $ Data.Map.lookup pos board
      pad k s = replicate (k - length s) ' ' ++ s
      showRow r = unwords [(pad (length $ show (n * m - 1)) . showEntry) (r, c) | c <- [0 .. n * m -1]]

-- | Creates a sudoku board from the given string.
readSudoku :: String -> Either String Sudoku
readSudoku text = case (filter (not . null) . map trim . lines) text of
  x : xs ->
    do
      (n, m) <- readDimensions x
      tokens <- readTokens n m xs
      entries <- mapM (readEntry n m) $ (filterTokens . flattenWithIndex) tokens
      sudoku <- foldM updateBoard (emptySudoku n m) entries
      Right sudoku
  _ ->
    do
      Left "missing lines"
  where
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

    readDimensions :: String -> Either String (RegionHeight, RegionWidth)
    readDimensions line = case mapM readEither $ words line of
      Right [x, y] | x > 0 && y > 0 -> Right (x, y)
      Right [_, _] -> Left "n and m must be positive integers"
      Right _ -> Left "n and m must be specified in the first line"
      Left _ -> Left $ "parse error: " ++ line

    readTokens :: RegionHeight -> RegionWidth -> [String] -> Either String [[String]]
    readTokens n m xs =
      if length xs /= n * m
        then do
          Left "invalid number of rows"
        else do
          let tokens = map words xs
          if any ((n * m /=) . length) tokens
            then Left "invalid number of columns"
            else Right tokens

    flattenWithIndex :: [[String]] -> [(Pos, String)]
    flattenWithIndex tokens = [((r, c), s) | (r, rs) <- zip [0 ..] tokens, (c, s) <- zip [0 ..] rs]

    filterTokens :: [(Pos, String)] -> [(Pos, String)]
    filterTokens tokens = [t | t@(_, s) <- tokens, s /= "."]

    readEntry :: RegionHeight -> RegionWidth -> (Pos, String) -> Either String (Pos, Entry)
    readEntry n m (pos, s) = case readEither s of
      Right x | 1 <= x && x <= n * m -> Right (pos, x - 1)
      Right _ -> Left $ "entry out of range: " ++ s
      Left _ -> Left $ "parse error: " ++ s

    updateBoard :: Sudoku -> (Pos, Entry) -> Either String Sudoku
    updateBoard sd (p, v) = case insert sd p v of
      Just s -> Right s
      Nothing -> Left "invalid board"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Checks if a number is within the valid range.
isValidNum :: RegionHeight -> RegionWidth -> Int -> Bool
isValidNum n m x = 0 <= x && x <= n * m

-- | Finds a region ID from a position on the board.
getRegionId :: Sudoku -> RowId -> ColumnId -> RegionId
getRegionId (Sudoku n m _) r c | all (isValidNum n m) [r, c] = r `div` n * n + c `div` m
getRegionId _ _ _ = undefined

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Inserts one entry to the given Sudoku instance.
insert :: Sudoku -> Pos -> Entry -> Maybe Sudoku
insert sd@(Sudoku n m (b, rows, cols, regs)) pos@(r, c) v
  | all (isValidNum n m) [v, r, c] =
    let rid = getRegionId sd r c
     in if pos `Data.Map.member` b || any (v `notMember`) [rows ! r, cols ! c, regs ! rid]
          then Nothing -- violating the constraints
          else Just $ Sudoku n m (Data.Map.insert pos v b, Data.Map.adjust (delete v) r rows, Data.Map.adjust (delete v) c cols, Data.Map.adjust (delete v) rid regs)
insert _ _ _ = Nothing

-- | Returns a list of empty positions.
getEmptyPositions :: Sudoku -> [Pos]
getEmptyPositions (Sudoku n m (b, _, _, _)) = [(r, c) | r <- [0 .. n * m - 1], c <- [0 .. n * m - 1], (r, c) `Data.Map.notMember` b]

-- | Returns a list of available numbers at the given position.
getAvailableNumbers :: Sudoku -> Pos -> [Int]
getAvailableNumbers sd@(Sudoku _ _ (_, rows, cols, regs)) (r, c) = toList $ foldl1 intersection [rows ! r, cols ! c, regs ! getRegionId sd r c]
