import           Control.Arrow ((>>>))
import           Data.List (sort)
import           Data.List.Split (splitOn)

main = interact $
  splitOn "," >>> map read >>> applyAll [solveA,solveB] >>> map show >>> unlines

solveA, solveB :: [Int] -> Int
solveA crabs = crabs >$> map (absdiff (median  crabs)) >>> sum
solveB crabs = crabs >$> applyAll [with averageL, with averageR] >>> map sum >>> minimum
  where
    with avg = map (tridiff (avg crabs))

median xs = sort xs !! (length xs `div` 2)
absdiff a b = abs (a - b)

-- Average rounded down
averageL xs = sum xs `div` length xs
-- Average rounded up
averageR xs = (sum xs + length xs - 1) `div` length xs

tridiff a b = tri (absdiff a b)
tri n = n*(n+1) `div` 2

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
