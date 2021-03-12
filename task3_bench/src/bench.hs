import Criterion.Main
import qualified Data.IntMap.Strict as Map
import Data.List (foldl', iterate')

adjustListElement :: (a -> a) -> Int -> [a] -> [a]
adjustListElement f i xs = case splitAt i xs of
  (a, x : b) -> a ++ (f x : b)
  _ -> xs

updateList :: Int -> Int -> [Int]
updateList n m =
  let ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
   in foldl' (\xs i -> adjustListElement (+ 1) i xs) (take n (repeat 0)) ys

updateList2 :: Int -> Int -> [Int]
updateList2 n m =
  let ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
   in foldl (\xs i -> adjustListElement (+ 1) i xs) (take n (repeat 0)) ys

updateIntMap :: Int -> Int -> Map.IntMap Int
updateIntMap n m =
  let ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
      xs0 = Map.fromList $ zip [0 .. (n - 1)] (repeat 0) :: Map.IntMap Int
   in foldl' (\xs i -> Map.adjust (+ 1) i xs) xs0 ys

updateIntMap2 :: Int -> Int -> Map.IntMap Int
updateIntMap2 n m =
  let ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
      xs0 = Map.fromList $ zip [0 .. (n - 1)] (repeat 0) :: Map.IntMap Int
   in foldl (\xs i -> Map.adjust (+ 1) i xs) xs0 ys

writeList_ :: [Int] -> Int -> [Int]
writeList_ xs m =
  let n = length xs
      ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
   in foldl' (\x i -> adjustListElement (+ 1) i xs) xs ys

readList_ :: [Int] -> Int -> [Int]
readList_ xs m =
  let n = length xs
      ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
   in map (xs !!) ys

writeIntMap :: Map.IntMap Int -> Int -> Map.IntMap Int
writeIntMap xs m =
  let n = length xs
      ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
   in foldl' (\x i -> Map.adjust (+ 1) i xs) xs ys

readIntMap :: Map.IntMap Int -> Int -> [Int]
readIntMap xs m =
  let n = length xs
      ys = take m $ iterate' (\x -> (x + 1000000007) `mod` n) 0
   in map xs Map.! ys

-- Our benchmark harness.
main :: IO ()
main = do
  let ls005 = updateList 5 10000
  let ls010 = updateList 10 10000
  let ls025 = updateList 25 10000
  let ls100 = updateList 100 10000
  let ls200 = updateList 200 10000
  let ls1k = updateList 1000 10000
  let ls2k = updateList 2000 10000
  let mp005 = updateIntMap 5 10000
  let mp010 = updateIntMap 10 10000
  let mp025 = updateIntMap 25 10000
  let mp100 = updateIntMap 100 10000
  let mp200 = updateIntMap 200 10000
  let mp1k = updateIntMap 1000 10000
  let mp2k = updateIntMap 2000 10000
  let m = 1000000

  defaultMain
    [ --bgroup
      -- "random update"
      -- [ bench "list n=5" $ whnf (updateList 5) 10000,
      --   bench "list n=10" $ whnf (updateList 10) 10000,
      --   bench "list n=100" $ whnf (updateList 100) 10000,
      --   -- bench "list n=5" $ whnf (updateList2 5) 10000,
      --   -- bench "list n=10" $ whnf (updateList2 10) 10000,
      --   -- bench "list n=100" $ whnf (updateList2 100) 10000,
      --   bench "IntMap n=5" $ whnf (updateIntMap 5) 10000,
      --   bench "IntMap n=10" $ whnf (updateIntMap 10) 10000,
      --   bench "IntMap n=100" $ whnf (updateIntMap 100) 10000
      --   -- bench "IntMap n=5" $ whnf (updateIntMap2 5) 10000,
      --   -- bench "IntMap n=10" $ whnf (updateIntMap2 10) 10000,
      --   -- bench "IntMap n=100" $ whnf (updateIntMap2 100) 10000
      -- ],
      bgroup
        "random write: List"
        [ bench "n=5" $ nf (writeList_ ls005) m,
          bench "n=10" $ nf (writeList_ ls010) m,
          bench "n=25" $ nf (writeList_ ls025) m,
          bench "n=100" $ nf (writeList_ ls100) m
          -- bench "List n=200" $ nf (readList_ ls200) m
          -- bench "List n=1000" $ nf (readList_ ls1k) m,
          -- bench "List n=2000" $ nf (readList_ ls2k) m
        ],
      bgroup
        "random write: IntMap"
        [ bench "n=5" $ nf (writeIntMap mp005) m,
          bench "n=10" $ nf (writeIntMap mp010) m,
          bench "n=25" $ nf (writeIntMap mp025) m,
          bench "n=100" $ nf (writeIntMap mp100) m
        ],
      bgroup
        "random read: List"
        [ bench "n=5" $ nf (readList_ ls005) m,
          bench "n=10" $ nf (readList_ ls010) m,
          bench "n=25" $ nf (readList_ ls025) m,
          bench "n=100" $ nf (readList_ ls100) m
          -- bench "List n=200" $ nf (readList_ ls200) m
          -- bench "List n=1000" $ nf (readList_ ls1k) m,
          -- bench "List n=2000" $ nf (readList_ ls2k) m
        ],
      bgroup
        "random read: IntMap"
        [ bench "n=5" $ nf (readIntMap mp005) m,
          bench "n=10" $ nf (readIntMap mp010) m,
          bench "n=25" $ nf (readIntMap mp025) m,
          bench "n=100" $ nf (readIntMap mp100) m
        ]
    ]
