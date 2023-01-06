#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package lens --package mtl --package generic-lens

import           Control.Arrow ((>>>))
import           Control.Monad (replicateM)
import           Data.List     (foldl', permutations)
import           Intcode

main = interact $
  readIntcode >>> applyAll [solveA, solveB] >>> map show >>> unlines

solveA :: [Int] -> Int
solveA prog = maximum (map runAmps phase)
  where
    phase = permutations [0..4]
    runAmps = foldl' (\signal ph -> head (runMachine prog [ph, signal])) 0

solveB :: [Int] -> Int
solveB = const 0

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
