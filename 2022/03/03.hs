#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow   ((>>>))
import           Data.Char       (isLower, ord)
import           Data.List       (intersect)
import           Data.List.Split (chunksOf)

main = interact $
  lines >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

solveA, solveB :: [String] -> Int
solveA = map (compartments >>> uncurry intersect >>> head >>> priority) >>> sum
solveB = chunksOf 3 >>> map (foldl1 intersect >>> head >>> priority) >>> sum

compartments s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27
