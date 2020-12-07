import           Control.Arrow

main = interact $
  lines >>> map read >>> applyAll [solveA, solveB] >>> map show >>> unlines

solveA, solveB :: [Int] -> Int
solveA = map fuel >>> sum
solveB = map (iterate fuel >>> drop 1 >>> takeWhile (>0) >>> sum) >>> sum

fuel x = max 0 ((x `div` 3) - 2)

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($a) fs
