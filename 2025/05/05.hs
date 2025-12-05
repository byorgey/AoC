#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package split

import Control.Arrow ((>>>))
import Data.List.Split (splitOn)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

data Input = Input {ranges :: [Interval], ingrs :: [Int]}

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> \[rs, is] -> Input (map readRange rs) (map read is)

readRange :: String -> Interval
readRange = splitOn "-" >>> map read >>> toPair >>> uncurry I

type Output = Int

solveA, solveB :: Input -> Output
solveA (Input rs is) = count (\i -> any (`containsI` i) rs) is
solveB (Input rs is) = rs >$> foldr insertRange [] >>> map sizeI >>> sum

insertRange :: Interval -> [Interval] -> [Interval]
insertRange i js | isEmpty i = js
insertRange i [] = [i]
insertRange i (j : js)
  | not (isEmpty (i ∩ j)) = insertRange (i ∪ j) js
  | otherwise = j : insertRange i js

------------------------------------------------------------
-- Utilities

infixr 0 >$>
(>$>) = flip ($)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

------------------------------------------------------------
-- Intervals

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

containsI :: Interval -> Int -> Bool
containsI i x = singletonI x ⊆ i

sizeI :: Interval -> Int
sizeI (I l h) = h - l + 1
