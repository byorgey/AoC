#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))
import Data.List (maximumBy)
import Data.Ord (Down (Down), comparing)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = [String]

readInput :: String -> Input
readInput = lines

type Output = Integer

solveA, solveB :: Input -> Output
solveA = map (bestPair >>> read) >>> sum
solveB = map (best 12 >>> read) >>> sum

bestPair :: String -> String
bestPair = (\(x, y) -> [x, y]) . maximum . pairs

best :: Int -> String -> String
best k ds = go k dsi
 where
  dsi = zip [0 ..] ds
  n = length ds

  go 0 _ = ""
  go j ps = c : go (j - 1) (dropWhile ((<= i) . fst) ps)
   where
    (i, c) = get (n - j + 1) ps

  -- Get the biggest, leftmost char + its index among the first j characters
  get j = maximumBy (comparing snd <> comparing (Down . fst)) . takeWhile ((< j) . fst)

------------------------------------------------------------
-- Utilities

maximumMay :: Ord a => [a] -> Maybe a
maximumMay = \case
  [] -> Nothing
  xs -> Just $ maximum xs

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
