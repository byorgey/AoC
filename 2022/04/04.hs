#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow   ((>>>))
import           Data.List.Split (splitOn)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Interval]]

readInput :: String -> Input
readInput = lines >>> map (splitOn "," >>> map (splitOn "-" >>> map read >>> \[l,h] -> I l h))

type Output = Int

solveA, solveB :: Input -> Output
solveA = count (\[i1,i2] -> (i1 ⊆ i2) || (i2 ⊆ i1))
solveB = count (\[i1,i2] -> not (isEmpty (i1 ∩ i2)))

-- Intervals

data Interval = I { lo :: Int, hi :: Int } deriving (Eq, Ord, Show)

(∪), (∩) :: Interval -> Interval -> Interval
I l1 h1 ∪ I l2 h2 = I (min l1 l2) (max h1 h2)
I l1 h1 ∩ I l2 h2 = I (max l1 l2) (min h1 h2)

isEmpty :: Interval -> Bool
isEmpty (I l h) = l > h

(⊆) :: Interval -> Interval -> Bool
i1 ⊆ i2 = i1 ∪ i2 == i2

-- Utility

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
