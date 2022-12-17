#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package MemoTrie --package array

{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed hiding ((!))
import qualified Data.Array.Unboxed as U
import           Data.Bits          (setBit, shiftL, testBit)
import           Data.Char          (isDigit)
import           Data.Int           (Int16)
import           Data.List          (find)
import           Data.Map           (Map, (!))
import qualified Data.Map.Strict    as M
import           Data.MemoTrie

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Node = String
data NodeInfo v = NI { rate :: Int16, idx :: Int, nbrs :: [v] } deriving (Eq, Show, Functor)
type Input v = Map v (NodeInfo v)

readInput :: String -> Input String
readInput = lines >>> map (words >>> readNode) >>> assignIndices >>> M.fromList

indexify :: Ord a => Input a -> Input Int
indexify ns = ns >$> M.mapKeys (ixMap!) >>> (fmap . fmap) (ixMap!)
  where
    ixMap = M.fromList (zip (M.keys ns) [0..])

readNode :: [String] -> (Node, NodeInfo String)
readNode (_:n:_:_:rt:(drop 4 -> ns)) = (n, NI (read (filter isDigit rt)) (-1) (map (filter (/=',')) ns))

-- Assign sequential indices to nodes with nonzero flow rate
assignIndices = go 0
  where
    go _ []                     = []
    go i (n@(_, NI 0 _ _) : ns) = n : go i ns
    go i ((n, ni) : ns)         = (n, ni { idx = i }) : go (i+1) ns

------------------------------------------------------------

-- Part 1: memoize the recurrence via MemoTrie
m1 :: Input String -> (String, Int16, Int) -> Int16
m1 g = m'
  where
    k = 1 + maximum (map idx (M.elems g))
    full = (1 `shiftL` k) - 1
    m' = memo f
    f (_, 0, _) = 0
    f (u, t, s)
      | s == full = 0
      | rate == 0 || s `testBit` idx = nbrMax
      | otherwise = max nbrMax ((t-1)*rate + m' (u, t-1, s `setBit` idx))
      where
        NI{..} = g!u
        nbrMax = maximum (map (\v -> m' (v, t-1, s)) nbrs)

-- Memoizing the Part 2 recurrence via MemoTrie uses too much memory.
m2 :: Input String -> ((String, String), Int16, Int) -> Int16
m2 g = m'
  where
    m' = memo (f2 g m')

-- The recurrence for Part 2.
f2 :: Ord v => Input v -> (((v,v), Int16, Int) -> Int16) -> (((v,v), Int16, Int) -> Int16)
f2 g _ ((_, _), 0, _) = 0
f2 g r ((u, v), t, s)
  | u > v = f2 g r ((v,u), t, s)
  | otherwise = maximum $
    -- both move to a new node
    [ r ((u', v'), t-1, s) | u' <- nbrs (g!u), v' <- nbrs (g!v) ]
    ++
    -- I open valve, elephant moves
    [ (t-1)*rate (g!u) + r ((u, v'), t-1, s `setBit` idx (g!u))
    | rate (g!u) > 0, not (s `testBit` idx (g!u)), v' <- nbrs (g!v) ]
    ++
    -- I move, elephant opens valve
    [ (t-1)*rate (g!v) + r ((u', v), t-1, s `setBit` idx (g!v))
    | rate (g!v) > 0, not (s `testBit` idx (g!v)), u' <- nbrs (g!u) ]
    ++
    -- We're at different nodes and both open valves
    [ (t-1)*(rate (g!u) + rate (g!v))
      + r ((u, v), t-1, s `setBit` idx (g!u) `setBit` idx (g!v))
    | u /= v, rate (g!u) > 0, rate (g!v) > 0
    , not (s `testBit` idx (g!u) || s `testBit` idx (g!v)) ]

-- Part 2.  A 60 * 60 * 2^15 DP table of Int16 takes about 225MB in
-- theory, we compute one time slice at a time.
m2b :: Input Int -> Int16
m2b g = snd (iterate step base !! 26) U.! (0,0,0)
  where
    n = maximum (M.keys g)
    k = 1 + maximum (map idx (M.elems g))
    full = (1 `shiftL` k) - 1
    base :: (Int16, UArray (Int,Int,Int) Int16)
    base = (0, accumArray const 0 ((0,0,0), (n,n,full)) [])

    step :: (Int16, UArray (Int,Int,Int) Int16) -> (Int16, UArray (Int,Int,Int) Int16)
    step (t,a) = (t+1, array ((0,0,0), (n,n,full))
      [ ((u,v,s), f2 g (\((u',v'),_,s') -> a U.! (u',v',s')) ((u,v),t+1,s))
      | u <- [0 .. n], v <- [0 .. n], s <- [0 .. full] ])

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input String -> Output
solveA = flip m1 ("AA", 30, 0) >>> fromIntegral
solveB = indexify >>> m2b >>> fromIntegral

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

------------------------------------------------------------

-- For visualizing the graphs
toDot :: Map Node (NodeInfo String) -> String
toDot g = unlines $
  [ "graph G {" ]
  ++
  map (\v -> v ++ " [label=" ++ v ++ ",shape=box" ++ fill v ++ "]") vertices
  ++
  map (\(a,b) -> a ++ " -- " ++ b ++ ";") edges
  ++
  [ "}" ]
  where
    fill v
      | v == "AA" = ",style=filled,fillcolor=blue"
      | rate (g!v) > 0 = ",style=filled,fillcolor=red"
      | otherwise = ""
    vertices = M.keys g
    edges = filter (uncurry (<)) . concatMap (\(u,ni) -> map (u,) (nbrs ni)) $ M.assocs g
