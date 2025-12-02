#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

import Control.Arrow ((>>>))
import Data.List (nub)
import Data.List.Split (chunksOf, splitOn)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = [(Integer, Integer)]

readInput :: String -> Input
readInput = splitOn "," >>> map (splitOn "-" >>> map read >>> toPair)

type Output = Integer

solveA, solveB :: Input -> Output
solveA = concatMap (get invalidA) >>> sum
solveB = concatMap (get invalidB) >>> sum

get :: (Integer -> Bool) -> (Integer, Integer) -> [Integer]
get invalid (x, y) = filter invalid [x .. y]

invalidA n = uncurry (==) (splitAt (l `div` 2) s)
 where
  s = show n
  l = length s

invalidB n = or [length (nub (chunksOf d s)) == 1 | d <- [1 .. l `div` 2], l `mod` d == 0]
 where
  s = show n
  l = length s

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)
