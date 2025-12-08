#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE ImportQualifiedPost #-}

import Control.Arrow ((>>>))
import Control.Monad (forM)
import Control.Monad.ST
import Data.Function (on)
import Data.List (sort)
import Data.List.Split
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import UnionFind qualified as UF

------------------------------------------------------------
-- Main & reading input

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Coords = [Int]
data Box = Box {boxID :: Int, coords :: Coords} deriving (Eq, Show)
type Input = [Box]

readInput :: String -> Input
readInput = lines >>> map (splitOn "," >>> map read) >>> zip [0 ..] >>> map (uncurry Box)

------------------------------------------------------------
-- Edges

data Edge = Edge Box Box deriving (Eq, Show)

distSq :: Coords -> Coords -> Int
distSq v1 v2 = sum (map (^ 2) (zipWith (-) v1 v2))

instance Ord Edge where
  compare = compare `on` (\(Edge b1 b2) -> distSq (coords b1) (coords b2))

prepEdges = pairs >>> map (uncurry Edge) >>> sort

------------------------------------------------------------
-- Solutions

solveA, solveB :: Input -> Int
solveA boxes = boxes >$> prepEdges >>> take 1000 >>> kruskalA (length boxes)
solveB boxes = boxes >$> prepEdges >>> kruskalB (length boxes) >>> wallDist
 where
  wallDist (Edge b1 b2) = head (coords b1) * head (coords b2)

connect :: UF.UnionFind s () -> Edge -> ST s ()
connect uf (Edge b1 b2) = UF.union uf (boxID b1) (boxID b2)

kruskalA :: Int -> [Edge] -> Int
kruskalA n es = runST $ do
  uf <- UF.new n ()
  mapM_ (connect uf) es
  sizes <- forM [0 .. n - 1] $ \i -> do
    s <- UF.size uf i
    u <- UF.find uf i
    pure (Down s, u)
  let top3 = take 3 (S.toList (S.fromList sizes))
  pure $ product (map (fst >>> getDown) top3)

kruskalB :: Int -> [Edge] -> Edge
kruskalB n es = runST $ do
  uf <- UF.new n ()
  go n uf es

go :: Int -> UF.UnionFind s () -> [Edge] -> ST s Edge
go !components uf (e@(Edge b1 b2) : es) = do
  c <- UF.connected uf (boxID b1) (boxID b2)
  if c
    then go components uf es
    else do
      connect uf e
      if components == 2 then pure e else go (components - 1) uf es

------------------------------------------------------------
-- Utilities

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
