#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow (second, (***), (>>>))

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

type Lights = Integer
type Input = [Machine]
data Machine = Machine {lights :: Lights, buttons :: [[Int]], joltage :: [Int]}
  deriving (Show)

readInput :: String -> Input
readInput = lines >>> map readMachine

readMachine :: String -> Machine
readMachine s = Machine (readLights w) (map readButton (init ws)) (readJoltage (last ws))
 where
  (w : ws) = words s

  snip = drop 1 . reverse . drop 1 . reverse

  readLights = snip >>> foldr (\b n -> (if b == '#' then 1 else 0) .|. (n `shiftL` 1)) 0
  readButton = snip >>> splitOn "," >>> map read
  readJoltage = snip >>> splitOn "," >>> map read

type Output = Int

solveA, solveB :: Input -> Output
solveA = map solveMachineA >>> sum
solveB = map solveMachineB >>> sum

-- Part A: BFS over bitvectors.  Search space is at most 2^10.
solveMachineA (Machine ls bs _) = pred . length $ bfs (== ls) next (S.singleton 0)
 where
  buttonLights = bs >$> map (map (1 `shiftL`) >>> foldl' (.|.) 0)
  next m = S.fromList $ map (m `xor`) buttonLights

-- Initial attempt via BFS, took forever and used up all my memory.
-- This makes sense, actually: if we have 10 counters each trying to
-- reach values in the 100's, the search space would be on the order
-- of 100^10, much too large.

-- solveMachineB (Machine _ bs js) = pred . length $ bfs (== js) next (S.singleton (map (const 0) js))
--  where
--   buttonCounters = map (elems . accumArray @UArray (\_ y -> y) 0 (0, length js - 1) . map (,1)) bs
--   next m = S.fromList . filter ok $ map (zipWith (+) m) buttonCounters
--   ok cs = and (zipWith (<=) cs js)

-- We're looking for positive k_i such that sum (k_i * c_i) = j where
-- c_i are the (0,1)-vectors each button, subject to the constraint
-- that sum k_i is as small as possible.  Sounds like ILP...
--
-- More specifically, let:
--   - B = the (0,1) matrix whose columns correspond to buttons
--   - k = a column vector representing the number of times we push each button
--   - j = a column vector representing the desired joltage counter values
--
-- Then we want to solve  B k = j  for k while minimizing the sum of k.
--
-- We can solve B k = j using Gaussian elimination; but what if there
-- are multiple solutions?  We're guaranteed there will be at least
-- one solution...
--
-- Ah, how about BSTA?  For each particular sum, we can just add an
-- extra equation specifying that the sum of all k = that sum.  Then
-- check if there is a solution or not.  Hmm, but binary search
-- doesn't work though, since this is not monotonic...

solveMachineB = undefined

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

singletonI :: Int -> Interval
singletonI i = I i i

(∈) :: Int -> Interval -> Bool
x ∈ i = singletonI x ⊆ i

sizeI :: Interval -> Int
sizeI (I l h) = h - l + 1

-- toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
-- toTable rng f = array rng (map (id &&& f) (range rng))

-- memo :: Ix i => (i,i) -> (i -> a) -> (i -> a)
-- memo rng = (!) . toTable rng

-- memoFix :: Ix i => (i,i) -> ((i -> a) -> (i -> a)) -> (i -> a)
-- memoFix rng f = fix (memo rng . f)

-- readParser p = parse p "" >>> either undefined id

type Coord = (Int, Int)

above, below, lt, rt :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
lt (r, c) = (r, c - 1)
rt (r, c) = (r, c + 1)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows@(r : _) = listArray ((0, 0), (length rows - 1, length r - 1)) (concat rows)

ixs :: (Ix i, IArray a e) => a i e -> [i]
ixs = range . bounds

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, lt, rt]

neighbors8 :: Coord -> [Coord]
neighbors8 = applyAll [above, below, lt, rt, above . lt, above . rt, below . lt, below . rt]

inGrid :: IArray UArray a => UArray Coord a -> [Coord] -> [Coord]
inGrid = filter . inRange . bounds

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
