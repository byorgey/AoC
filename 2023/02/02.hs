#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((>>>))
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map.Strict qualified as M

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (a : as) = f a : as

data Color = Red | Green | Blue deriving (Eq, Ord, Show, Read)
type CubeSet = Map Color Int
type Game = (Int, [CubeSet])
type Input = [Game]

readInput :: String -> Input
readInput = lines >>> map readGame

readGame :: String -> Game
readGame s = (read (last (words label)), cubeSets)
 where
  (label, drop 2 -> gs) = break (== ':') s
  cubeSets = map readCubeSet (splitOn "; " gs)

readCubeSet :: String -> CubeSet
readCubeSet = M.fromList . map readColor . splitOn ", "
 where
  readColor = words >>> \[n, c] -> (read (onHead toUpper c), read n)

bagA :: CubeSet
bagA = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

possible :: CubeSet -> CubeSet -> Bool
possible bag cubes = all (\(c, n) -> n <= bag M.! c) (M.assocs cubes)

solveA, solveB :: Input -> Int
solveA = filter (snd >>> all (possible bagA)) >>> map fst >>> sum
solveB = map (snd >>> M.unionsWith max >>> M.elems >>> product) >>> sum
