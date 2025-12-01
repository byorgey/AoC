#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array --package mtl

import Control.Arrow ((>>>))
import Control.Monad.State

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Turn = (Dir, Int)
data Dir = L | R deriving (Eq, Ord, Show, Read)
type Input = [Turn]

readInput :: String -> Input
readInput = lines >>> map readTurn

readTurn :: String -> Turn
readTurn (d : n) = (read [d], read n)

type Output = Int

solveA, solveB :: Input -> Output
solveA ts = count (== 0) $ evalState (mapM turnA ts) 50
solveB ts = count (== 0) . concat $ evalState (mapM turnB ts) 50

------------------------------------------------------------

turnA :: Turn -> State Int Int
turnA (d, n) = modify ((`mod` 100) . (if d == R then (+ n) else subtract n)) >> get

turnB :: Turn -> State Int [Int]
turnB (d, n) = do
  dial <- get
  let ds = take n . drop 1 $ iterate ((`mod` 100) . (if d == R then succ else pred)) dial
  put (last ds)
  pure ds

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
