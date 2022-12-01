#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package split

import           Control.Arrow   ((>>>))
import           Data.List       (sortOn)
import           Data.List.Split (splitOn)
import           Data.Ord

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = [[Int]]

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> map (map read)

type Output = Int

solveA, solveB :: Input -> Output
solveA = map sum >>> maximum
solveB = map sum >>> sortOn Down >>> take 3 >>> sum
