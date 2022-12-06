#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow ((>>>))
import           Data.List     (findIndex, tails)
import           Data.Maybe    (fromJust)
import qualified Data.Set      as S

main = interact $
  applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = String
type Output = Int

solveA, solveB :: Input -> Output
solveA = solve 4
solveB = solve 14

solve k = tails >>> map (take k) >>> findIndex (ok k) >>> fromJust >>> (+k)

ok k = S.fromList >>> S.size >>> (==k)
