#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE ImportQualifiedPost #-}

import Control.Arrow ((>>>))
import Data.List (delete)
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Set qualified as S

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Device = String
type Input = Map Device [Device]

readInput :: String -> Input
readInput = lines >>> map (words >>> (\(d : ds) -> (init d, ds))) >>> M.fromList

type Output = Int

solveA, solveB :: Input -> Output
solveA g = pathsTo g "out" ! "you"
solveB g = totalPaths ["svr", "dac", "fft", "out"] + totalPaths ["svr", "fft", "dac", "out"]
 where
  totalPaths waypoints = product (zipWith (\from to -> pathsTo g to ! from) waypoints (tail waypoints))

pathsTo :: Input -> Device -> Map Device Int
pathsTo g tgt = pathsFrom
 where
  pathsFrom = M.fromList $ (tgt, 1) : [(d, countPaths d) | d <- delete tgt devices]
  countPaths d = sum (maybe [] (map (pathsFrom !)) (g !? d))
  devices = S.toList . S.fromList $ M.keys g ++ concat (M.elems g)

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
