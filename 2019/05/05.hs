#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package lens --package mtl --package generic-lens

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Applicative
import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Intcode

main = interact $
  readIntcode >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

type Output = Int

solveA, solveB :: Input -> Output
solveA = initMachineIO [1] >>> execState run >>> output >>> head
solveB = initMachineIO [5] >>> execState run >>> output >>> head

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
