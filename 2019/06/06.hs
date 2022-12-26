#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow   (second, (&&&), (>>>))
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Tree       (Tree (Node, rootLabel), foldTree, levels)
import           Data.Tuple      (swap)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [(String, String)]

readInput :: String -> Input
readInput = lines >>> map (splitOn ")" >>> toPair)

------------------------------------------------------------

-- Given a list of (child,parent) pairs, build the corresponding forest.
forestFromParents :: Ord a => [(a,a)] -> [Tree a]
forestFromParents ps = map (treeMap M.!) (S.toList roots)
  where
    parents = ps >$> map (swap >>> second S.singleton) >>> M.fromListWith S.union
    children = map fst ps
    roots = M.keysSet parents `S.difference` S.fromList children
    treeMap = M.mapWithKey (\p cs -> Node p (cs >$> S.toList >>> map (treeMap M.!))) parents
      `M.union` (children >$> map (id &&& flip Node []) >>> M.fromList)

-- If we know for sure there is a single tree.
treeFromParents :: Ord a => [(a,a)] -> Tree a
treeFromParents = forestFromParents >>> head

annotate :: (a -> [b] -> b) -> Tree a -> Tree (a,b)
annotate f = foldTree (\a ts -> Node (a, f a (map (rootLabel >>> snd) ts)) ts)

type Output = Int

solveA, solveB :: Input -> Output
solveA = map swap >>> treeFromParents >>> levels >>> zipWith (\h as -> h * length as) [0..] >>> sum
solveB ps = go youP (depth youP) sanP (depth sanP)
  where
    parent = ps >$> map swap >>> M.fromList
    youP = parent M.! "YOU"
    sanP = parent M.! "SAN"
    depth "COM" = 0
    depth n     = 1 + depth (parent M.! n)

    go l ld r rd
      | ld > rd = 1 + go (parent M.! l) (ld-1) r rd
      | rd > ld = 1 + go l ld (parent M.! r) (rd-1)
      | l == r  = 0
      | otherwise = 2 + go (parent M.! l) (ld-1) (parent M.! r) (rd-1)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a,a)
toPair [x,y] = (x,y)

infixr 0 >$>
(>$>) = flip ($)
