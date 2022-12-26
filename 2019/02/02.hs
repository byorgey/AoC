#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package lens --package generic-lens --package mtl

{-# LANGUAGE DataKinds #-}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.List           (find)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust)
import           Intcode

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = splitOn "," >>> map read

type Output = Int

runWith n v = initMachine >>> evalState (setNounVerb n v >> run >> rd 0)

solveA, solveB :: Input -> Output
solveA = runWith 12 2
solveB m = find' (\(n,v) -> runWith n v m == 19690720) [(n,v) | n <- [0..99], v <- [0..99]] >$> (\(n,v) -> 100 * n + v)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)
