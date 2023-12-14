#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow ((>>>))

-- import           Control.Lens
-- import           Control.Monad.State
-- import           Control.Monad.Writer
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple

-- import           Text.Parsec          hiding (State)
-- import           Text.Parsec.String
import Text.Printf

import Debug.Trace

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = ()

readInput :: String -> Input
readInput = lines >>> _

type Output = Int

solveA, solveB :: Input -> Output
solveA = const 0
solveB = const 0

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
 where
  bfs' seen layer
    | S.null layer = []
    | any isGoal layer = [layer]
    | otherwise = layer : bfs' seen' layer'
   where
    layer' = (foldMap next layer) `S.difference` seen'
    seen' = S.union seen layer

dfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> a -> [[a]]
dfs winning fnext start = dfs' S.empty [start] start
 where
  dfs' visited path cur
    | winning cur = [path]
    | otherwise = concatMap (\n -> dfs' (S.insert n visited) (n : path) n) next
   where
    next = fnext cur

uncurryL :: (a -> a -> b) -> [a] -> b
uncurryL f [x, y] = f x y

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = map (x :) (choose (k - 1) xs) ++ choose k xs

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

cardinality :: Ord a => [a] -> Map a Int
cardinality = map (,1) >>> M.fromListWith (+)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (a : as) = f a : as

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)

data Interval = I {lo :: Int, hi :: Int} deriving (Eq, Ord, Show)

(∪), (∩) :: Interval -> Interval -> Interval
I l1 h1 ∪ I l2 h2 = I (min l1 l2) (max h1 h2)
I l1 h1 ∩ I l2 h2 = I (max l1 l2) (min h1 h2)

isEmpty :: Interval -> Bool
isEmpty (I l h) = l > h

(⊆) :: Interval -> Interval -> Bool
i1 ⊆ i2 = i1 ∪ i2 == i2

-- toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
-- toTable rng f = array rng (map (id &&& f) (range rng))

-- memo :: Ix i => (i,i) -> (i -> a) -> (i -> a)
-- memo rng = (!) . toTable rng

-- memoFix :: Ix i => (i,i) -> ((i -> a) -> (i -> a)) -> (i -> a)
-- memoFix rng f = fix (memo rng . f)

-- readParser p = parse p "" >>> either undefined id

type Coord = (Int, Int)

above, below, left, right :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

-- mkArray :: IArray UArray a => [[a]] -> UArray Coord a
-- mkArray rows = listArray ((0,0), (length rows - 1, length (head rows) - 1)) (concat rows)

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, left, right]

neighborsIn :: IArray UArray a => UArray Coord a -> Coord -> [Coord]
neighborsIn a = filter (inRange (bounds a)) . neighbors

zipWithL :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithL _ [] ys = ys
zipWithL _ xs [] = xs
zipWithL f (x : xs) (y : ys) = f x y : zipWithL f xs ys

zipSum :: [[Int]] -> [Int]
zipSum [] = []
zipSum ([] : xss) = 0 : zipSum xss
zipSum ((x : xs) : xss) = x : zipWithL (+) xs (zipSum xss)

-- Floyd's cycle-finding algorithm.  floyd f x0 returns the smallest
-- (μ, λ) such that (s !! (μ + i) == s !! (μ + i + λ)) for all i >= 0
-- where s = iterate f x0.
floyd :: Eq a => (a -> a) -> a -> (Int, Int)
floyd f x0 = (μ, λ)
 where
  tortoiseHare = f *** (f . f)
  step = f *** f

  findMatch :: Eq a => ((a, a) -> (a, a)) -> (a, a) -> (Int, a)
  findMatch g = iterate g >>> zip [0 ..] >>> find' (snd >>> uncurry (==)) >>> second fst

  (_, xν) = findMatch tortoiseHare (tortoiseHare (x0, x0))
  (μ, xμ) = findMatch step (x0, xν)
  λ = succ . fromJust $ elemIndex xμ (iterate f (f xμ))
