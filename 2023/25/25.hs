#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow (second, (>>>))
import Data.List ((\\))
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)

main = interact $ readInput >>> solve >>> show

type Component = String
type Edge = (Component, Component)
type Input = [Edge]

readInput :: String -> Input
readInput = lines >>> concatMap readEdges

readEdges :: String -> [Edge]
readEdges = filter (/= ':') >>> words >>> \(c : cs) -> map (c,) cs

mkGraph :: Ord a => [(a, a)] -> Map a [a]
mkGraph es = (es ++ map swap es) >$> map (second (: [])) >>> M.fromListWith (++)

edgesToDot :: [Edge] -> String
edgesToDot es =
  unlines $
    ["graph G {"]
      ++ vertices
      ++ map (\(a, b) -> a ++ " -- " ++ b ++ ";") es
      ++ ["}"]
 where
  vertices = S.toList . S.fromList $ map fst es ++ map snd es

-- Edges to disconnect, discovered via drawing clustered graph with sfdp
toDisconnect :: [Edge]
toDisconnect = es ++ map swap es
 where
  es = [("tvf", "tqn"), ("vzb", "tnr"), ("lmg", "krx")]

twoComponents :: Ord a => Map a [a] -> [Int]
twoComponents g = [c1, n - c1]
 where
  c1 = S.size . S.unions $ bfs (const False) ((g M.!) >>> S.fromList) (S.singleton (head (M.keys g)))
  n = length $ M.keys g

type Output = Int

solve :: Input -> Output
solve = (\\ toDisconnect) >>> mkGraph >>> twoComponents >>> product

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> Set a) -> Set a -> [Set a]
bfs isGoal next = bfs' S.empty
 where
  bfs' seen layer
    | S.null layer = []
    | any isGoal layer = [layer]
    | otherwise = layer : bfs' seen' layer'
   where
    layer' = foldMap next layer `S.difference` seen'
    seen' = S.union seen layer

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
