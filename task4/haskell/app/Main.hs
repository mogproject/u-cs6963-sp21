module Main where

import Data.Bits (Bits (), complement, countTrailingZeros, popCount, shift, xor, (.&.), (.|.))
import qualified Data.Heap as H
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Vector.Unboxed (Vector, toList, (!), (//))
import qualified Data.Vector.Unboxed as V
import System.IO (BufferMode (LineBuffering), hSetBuffering, isEOF, stdout)
import System.Random (StdGen, getStdGen, mkStdGen)
import Text.Read (readMaybe)
import Data.List (foldl')

--------------------------------------------------------------------------------
-- Data structures
--------------------------------------------------------------------------------
type Vertex = Int

type Edge = (Vertex, Vertex)

-- Only supports graphs with n <= 64
type Bitmap = Int64

type Matrix = Vector Bitmap

emptyBitmap :: Bitmap
emptyBitmap = 0

emptyMatrix :: Matrix
emptyMatrix = V.replicate 64 0

identityMatrix :: Matrix
identityMatrix = V.generate 64 bx

--------------------------------------------------------------------------------
-- Graph
--------------------------------------------------------------------------------
data Graph = Graph
  { numEdited :: Int,
    vertices :: Bitmap,
    nbr1 :: Matrix,
    nbr2 :: Matrix,
    perm :: Matrix
  }

emptyGraph :: Graph
emptyGraph =
  Graph
    { numEdited = 0,
      vertices = emptyBitmap,
      nbr1 = emptyMatrix,
      nbr2 = emptyMatrix,
      perm = emptyMatrix
    }

degree :: Graph -> Vertex -> Int
degree Graph {nbr1 = r1} v = bcnt $ r1 ! v

-- | Closed neighborhood radius=2
closedNbr2 :: Matrix -> Matrix
closedNbr2 r1 = V.generate (V.length r1) (\v -> foldl' (\b x -> b .|. r1 ! x) emptyBitmap (bxs (r1 ! v)))

constructGraph :: Int -> [Edge] -> Graph
constructGraph n es =
  let vs = bx n - 1
      r1 = foldl' (\m (v, u) -> morxy' m v u) emptyMatrix es
      r2 = closedNbr2 r1 `mminus` r1 `mminus` identityMatrix
      pm = V.zipWith (.|.) r1 r2
   in emptyGraph {vertices = vs, nbr1 = r1, nbr2 = r2, perm = pm}

findP3 :: Graph -> Maybe (Vertex, Vertex, Vertex)
findP3 Graph {vertices = vs, nbr1 = r1, nbr2 = r2} =
  case [(v, head (bxs us)) | v <- bxs vs, let us = r2 ! v, us /= 0] of
    (v, u) : _ -> Just (v, head (bxs ((r1 ! v) .&. (r1 ! u))), u)
    _ -> Nothing

-- | from the given endpoint
findP3' :: Graph -> Vertex -> Maybe (Vertex, Vertex, Vertex)
findP3' Graph {vertices = vs, nbr1 = r1, nbr2 = r2} v =
  case r2 ! v of
    0 -> Nothing
    us ->
      let u = head (bxs us)
       in Just (v, head (bxs ((r1 ! v) .&. (r1 ! u))), u)

-- | find any P_3 including v
findP3'' :: Graph2 -> Vertex -> Maybe (Vertex, Vertex, Vertex)
findP3'' Graph2 {degm = dm, g2nbr1 = r1, g2nbr2 = r2} v =
  case r2 ! v of
    0 ->
      -- u <- N(v), W = N(v) /\ N^2(u) => u-v-w induces P_3
      case [(u, ws) | u <- bxs (r1 ! v), let ws = (r1 ! v) .&. (r2 ! u), ws /= 0] of
        [] -> Nothing
        (u, ws) : _ -> Just (u, v, head (bxs ws))
    us ->
      let u = head (bxs us)
       in Just (v, head (bxs ((r1 ! v) .&. (r1 ! u))), u)

middleVertices :: Matrix -> Vertex -> Vertex -> Bitmap
middleVertices r1 v u = (r1 ! v) .&. (r1 ! u)

canEdit :: Edge -> Graph -> Bool
canEdit (v, u) Graph {perm = pm} = mget pm v u

editEdge :: Edge -> Graph -> Graph
editEdge (v, u) g@Graph {numEdited = ne, vertices = vs, nbr1 = r1, nbr2 = r2, perm = pm}
  | mget r1 v u -- edge deletion
    =
    let r1' = mminusxy' r1 v u
        -- edge v-u: distance 1 -> 2 or >=3
        pm' = mminusxy' pm v u
        r2' = if middleVertices r1' v u /= 0 then morxy' r2 v u else r2
        -- w-v-u or w-u-v is the only distance-2 path
        sep1 = [(w, u) | w <- bxs ((r1' ! v) .&. (r2 ! u)), middleVertices r1' w u == 0]
        sep2 = [(v, w) | w <- bxs ((r2 ! v) .&. (r1' ! u)), middleVertices r1' v w == 0]
        (r2'', pm'') = foldl' (\(m1, m2) (i, j) -> (mminusxy' m1 i j, mminusxy' m2 i j)) (r2', pm) (sep1 ++ sep2)
     in g {numEdited = ne + 1, nbr1 = r1', nbr2 = r2'', perm = pm''}
  | otherwise -- edge addition
    =
    let r1' = mxorxy' r1 v u
        -- edge v-u: distance >=2 -> 1
        pm' = mminusxy' pm v u
        r2' = mminusxy' r2 v u
        -- if u and w are not adjacent, then w-u becomes distance 2
        sep1 = [(w, u) | w <- bxs ((r1' ! v) `bminus` (r1' ! u) `bminusx` u)]
        sep2 = [(v, w) | w <- bxs ((r1' ! u) `bminus` (r1' ! v) `bminusx` v)]
        r2'' = foldl' (\m (i, j) -> morxy' m i j) r2' (sep1 ++ sep2)
     in g {numEdited = ne + 1, nbr1 = r1', nbr2 = r2'', perm = pm'}

lockEdge :: Graph -> Edge -> Graph
lockEdge g@Graph {perm = pm} (v, u) = g {perm = mminusxy' pm v u}

-- | Vertex mask, number of edges
type ComponentInfo = (Bitmap, Int)

components :: Graph -> [ComponentInfo]
components g@Graph {vertices = vs, nbr1 = r1} =
  reverse $ components' (bxs vs) emptyBitmap []
  where
    bfs :: Vertex -> ComponentInfo
    bfs root = bfs' (bx root) emptyBitmap 0

    bfs' :: Bitmap -> Bitmap -> Int -> ComponentInfo
    bfs' frontier visited m | frontier == emptyBitmap = (visited, m `div` 2)
    bfs' frontier visited m =
      let v = head $ bxs frontier
          visited' = borx visited v
          nbr = r1 ! v
          m' = m + bcnt nbr
          frontier' = (frontier .|. nbr) `bminus` visited'
       in bfs' frontier' visited' m'

    components' :: [Vertex] -> Bitmap -> [ComponentInfo] -> [ComponentInfo]
    components' [] visited sofar = sofar
    components' (u : us) visited sofar | bget visited u = components' us visited sofar
    components' (u : us) visited sofar =
      let result = bfs u
       in components' us (visited .|. fst result) (result : sofar)

numVertices :: Graph -> Int
numVertices Graph {vertices = vs} = bcnt vs

numEdges :: Graph -> Int
numEdges Graph {vertices = vs, nbr1 = r1} = V.sum (mmapWithFilter' (const 0) bcnt vs r1) `div` 2

--------------------------------------------------------------------------------
data Graph2 = Graph2
  { degm :: Matrix, -- degree matrix
    g2nbr1 :: Matrix,
    g2nbr2 :: Matrix
  }

-- | Complexity: O(nL)
fromGraph :: Graph -> Graph2
fromGraph g@Graph {vertices = vs, nbr1 = r1, nbr2 = r2} =
  let dm = foldl' (\m v -> morxy m (degree g v) v) emptyMatrix (bxs vs)
   in Graph2 {degm = dm, g2nbr1 = r1, g2nbr2 = r2}

-- | Choose a vertex with the minimum degree.
-- | Complexity: O(nL)
g2Vertices :: Graph2 -> [Vertex]
g2Vertices g = map snd $ mxs' (degm g)

crossMatrix :: Int -> Matrix
crossMatrix k = V.generate 64 (\i -> if i == k then complement emptyBitmap else bx k)

removeVertex :: Graph2 -> Vertex -> Graph2
removeVertex g@Graph2 {degm = dm, g2nbr1 = r1, g2nbr2 = r2} v =
  let us = [(bcnt (r1 ! u), u) | u <- bxs (r1 ! v)] -- (d(u), u) where u <- N(v)
      dm' = mminusxy dm (bcnt (r1 ! v)) v -- v: degree d(v) to 0
      dm'' = foldl' (\m (d, u) -> mxorxy (mxorxy m d u) (d -1) u) dm' us -- u <- N(v): degree d(u) to d(u)-1
      r1' = r1 `mminus` crossMatrix v
      r2' = r2 `mminus` crossMatrix v
      r2'' = r2' `mminus` mfromList' [(u, w) | u <- bxs (r1 ! v), w <- bxs ((r2 ! u) .&. (r1 ! v)), u < w, (r1 ! u) .&. (r1 ! w) == bx v]
   in Graph2 {degm = dm'', g2nbr1 = r1', g2nbr2 = r2''}

--------------------------------------------------------------------------------
-- Bit operations
--------------------------------------------------------------------------------

-- | Singleton
bx :: Int -> Bitmap
bx x = 1 `shift` x

bget :: Bitmap -> Int -> Bool
bget b x = (b .&. bx x) /= 0

bcnt :: Bitmap -> Int
bcnt = popCount

bminus :: Bitmap -> Bitmap -> Bitmap
bminus b c = b .&. complement c

bminusx :: Bitmap -> Int -> Bitmap
bminusx b x = bminus b (bx x)

bandx :: Bitmap -> Int -> Bitmap
bandx b x = b .&. bx x

borx :: Bitmap -> Int -> Bitmap
borx b x = b .|. bx x

bxorx :: Bitmap -> Int -> Bitmap
bxorx b x = b `xor` bx x

bissubset :: Bitmap -> Bitmap -> Bool
bissubset a b = a .&. b == a

-- | bitmap to list
bxs :: Bitmap -> [Int]
bxs b = tail . map fst $ takeWhile ((/= -1) . fst) $ iterate f (0, b)
  where
    f (_, y) =
      if y == 0
        then (-1, 0)
        else
          let p = countTrailingZeros y
           in (p, y `xor` (1 `shift` p))

mcnt :: Matrix -> Int
mcnt = V.sum . V.map bcnt

mor :: Matrix -> Matrix -> Matrix
mor = V.zipWith (.|.)

mminus :: Matrix -> Matrix -> Matrix
mminus = V.zipWith bminus

mfxy :: (Bitmap -> Int -> Bitmap) -> Matrix -> Int -> Int -> Matrix
mfxy f m i j = m // [(i, f (m ! i) j)]

-- | symmetric update
mfxy' :: (Bitmap -> Int -> Bitmap) -> Matrix -> Int -> Int -> Matrix
mfxy' f m i j = m // [(i, f (m ! i) j), (j, f (m ! j) i)]

morxy :: Matrix -> Int -> Int -> Matrix
morxy = mfxy borx

morxy' :: Matrix -> Int -> Int -> Matrix
morxy' = mfxy' borx

mxor :: Matrix -> Matrix -> Matrix
mxor = V.zipWith xor

mxorxy :: Matrix -> Int -> Int -> Matrix
mxorxy = mfxy bxorx

mxorxy' :: Matrix -> Int -> Int -> Matrix
mxorxy' = mfxy' bxorx

mminusxy :: Matrix -> Int -> Int -> Matrix
mminusxy = mfxy bminusx

mminusxy' :: Matrix -> Int -> Int -> Matrix
mminusxy' = mfxy' bminusx

-- | matrix to list, simplifying symmetry
mxs :: Matrix -> [(Int, Int)]
mxs m = [(v, u) | v <- [0 .. V.length m - 1], u <- bxs (m ! v), v < u]

mxs' :: Matrix -> [(Int, Int)]
mxs' m = [(v, u) | v <- [0 .. V.length m - 1], u <- bxs (m ! v)]

mfromList' :: [(Int, Int)] -> Matrix
mfromList' = foldl' (\m (i, j) -> morxy' m i j) emptyMatrix

mget :: Matrix -> Int -> Int -> Bool
mget m i = bget (m ! i)

-- | map bitmaps only for the given indices
mmapWithFilter :: (Bitmap -> Bitmap) -> Bitmap -> Matrix -> Vector Bitmap
mmapWithFilter = mmapWithFilter' id

mmapWithFilter' :: (V.Unbox a) => (Bitmap -> a) -> (Bitmap -> a) -> Bitmap -> Matrix -> Vector a
mmapWithFilter' zeromap f target = V.imap (\i -> if bget target i then f else zeromap)

--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------
readGraph :: [String] -> Graph
readGraph (s : ss) =
  let n = read $ words s !! 2
      es = map readEdge ss
   in constructGraph n es
readGraph _ = emptyGraph

readEdge :: String -> Edge
readEdge s =
  let xs = words s
   in (read (head xs) - 1, read (xs !! 1) - 1)

showEdge :: Edge -> String
showEdge (i, j) = show (i + 1) ++ " " ++ show (j + 1)

--------------------------------------------------------------------------------
-- Branch and bound
--------------------------------------------------------------------------------

reduceAndLock :: Graph -> Graph
reduceAndLock = reduceVertices . reduceTwins . reduceNonEdges

-- | uv is permanent non-edge /\ vw is permanent non-edge => uw is permanent non-edge
reduceNonEdges :: Graph -> Graph
reduceNonEdges g@Graph {vertices = vs, perm = pm} =
  let editableEdges = [(v, u) | v <- bxs vs, u <- bxs (pm ! v)]
   in foldl' f g editableEdges
  where
    f g'@Graph {nbr1 = r1', perm = pm'} (v, u) =
      let x = (r1' ! v) .&. complement (pm' ! v)
          y = complement (r1' ! u) .&. complement (pm' ! u)
          z = bminusx (bminusx (x .&. y) v) u
       in case z of
            0 -> g'
            _ | mget r1' v u -> editEdge (v, u) g'
            _ -> lockEdge g' (v, u)

reduceTwins :: Graph -> Graph
reduceTwins g@Graph {vertices = vs, nbr1 = r1, perm = pm} =
  let twins = [(v, u) | v <- bxs vs, u <- bxs (r1 ! v), v < u, borx (r1 ! v) v == borx (r1 ! u) u]
   in foldl' lockEdge g twins

reduceVertices :: Graph -> Graph
reduceVertices g@Graph {vertices = vs, perm = pm} =
  let vs' = foldl' bxorx 0 [v | v <- bxs vs, pm ! v /= 0]
   in g {vertices = vs'}

lowerbound :: Graph -> Int
lowerbound = packP3

packP3 :: Graph -> Int
packP3 g = packP3' (fromGraph g) (vertices g) 0

packP3' :: Graph2 -> Bitmap -> Int -> Int
packP3' g vs sofar = case [v | v <- g2Vertices g, bget vs v] of
  v_ : _ -> case findP3'' g v_ of
    Just (v, u, w) ->
      let g' = foldl' removeVertex g [v, u, w]
       in packP3' g' (foldl' bminusx vs [v, u, w]) (sofar + 1)
    Nothing -> packP3' g (bminusx vs v_) sofar -- same graph, remove candidates
  [] -> sofar

-- greedy matching,
packP2 :: Graph -> (Int, Matrix)
packP2 g@Graph {vertices = vs, nbr1 = r1} =
  let result = packP2' (fromGraph g) emptyMatrix
   in (mcnt result `div` 2, result)

packP2' :: Graph2 -> Matrix -> Matrix
packP2' g@Graph2 {g2nbr1 = r1} sofar = case g2Vertices g of
  v : _ -> case bxs (r1 ! v) of
    u : _ ->
      -- remove v-u
      let g' = foldl' removeVertex g [v, u]
       in packP2' g' (morxy' sofar v u)
    [] ->
      -- v is isolated
      packP2' (removeVertex g v) sofar
  [] -> sofar

upperbound :: Graph -> (Int, Matrix)
upperbound g@Graph {nbr1 = r1} =
  foldl' f (0, r1) (components g)
  where
    f (cnt, adj) (vs, m) =
      let n = bcnt vs -- number of vertices in the component
          m' = n * (n - 1) `div` 2 - m -- number of missing edges in the component
      --     h = if m < m'
      --       then makeEmpty vs adj
      --       else makeClique vs adj
      --  in (cnt + min m m', h)
          (p, matching) = packP2 g {vertices = vs}
          h = if m - p < m'
            then mor (mmapWithFilter (`bminus` vs) vs adj) matching -- keep matchings only
            else makeClique vs adj
      in (cnt + min (m - p) m', h)

makeEmpty :: Bitmap -> Matrix -> Matrix
makeEmpty = mmapWithFilter (const 0)

makeClique :: Bitmap -> Matrix -> Matrix
makeClique component m = mminus (mmapWithFilter (.|. component) component m) identityMatrix

branch :: Graph -> [Graph]
branch g = case findP3 g of
  Just (v, u, w) -> mapMaybe (\e -> if canEdit e g then Just (editEdge e g) else Nothing) [(v, u), (v, w), (u, w)]
  Nothing -> []

solve :: Graph -> [Edge]
solve g@Graph {nbr1 = r1} = mxs . mxor r1 . search 2000000000 emptyMatrix $ [g]

search :: Int -> Matrix -> [Graph] -> Matrix
search best sofar (g : gs) =
  let h = reduceAndLock g
      lb = lowerbound h
      lb' = numEdited h + lb
      (ub, cert) = upperbound h
      br = branch h
      result
        | numEdited g >= best || numEdited h >= best || lb' >= best = search best sofar gs -- branch cut
        | lb == ub = search lb' cert gs -- update the best
        | otherwise = search best sofar (reverse br ++ gs) -- branching
   in result
search _ sofar _ = sofar

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------
main :: IO ()
main = do
  contents <- getContents
  putStr . unlines . map showEdge . solve . readGraph . lines $ contents
