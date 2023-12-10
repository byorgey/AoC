#!/usr/bin/env stack
-- stack --resolver lts-19.28 script

import Control.Category ((>>>))

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = [[Int]]

readInput :: String -> Input
readInput = lines >>> map (words >>> map read)

babbage (x : xs)
  | all (== 0) (x : xs) = repeat 0
  | otherwise = scanl (+) x (babbage (diff (x : xs)))

diff xs = zipWith (-) (tail xs) xs

next as = babbage as !! length as

solveA, solveB :: Input -> Int
solveA = map next >>> sum
solveB = map (reverse >>> next) >>> sum
