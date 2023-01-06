#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array --package lens --package mtl --package generic-lens

import Intcode
import           Control.Arrow       ((>>>))

main = interact $
  readIntcode >>> applyAll [solveA,solveB] >>> unlines

solveA, solveB :: [Int] -> String
solveA prog = show (runMachine prog [1])
solveB prog = show (runMachine prog [2])

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
