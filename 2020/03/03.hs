import           Control.Arrow
import           Data.Array.Unboxed

main = interact $
  lines >>> mkArray >>> runAll [solveA, solveB] >>> map show >>> unlines

runAll fs a = map ($a) fs

mkArray :: [String] -> UArray (Int,Int) Char
mkArray rows = listArray ((0,0),(r-1,c-1)) (concat rows)
  where
    r = length rows
    c = length (head rows)

infixl 0 >$>
(>$>) = flip ($)

solveA = countTrees (1,3)
solveB a = product $ map (flip countTrees a) [(1,1), (1,3), (1,5), (1,7), (2,1)]

countTrees :: (Int,Int) -> UArray (Int,Int) Char -> Int
countTrees (dr,dc) a = iterate step (0,0) >$> takeWhile (fst >>> (<= maxR)) >>> count tree
  where
    (_,(maxR, maxC)) = bounds a
    step (r,c) = (r+dr, (c+dc) `mod` (maxC + 1))

    tree (r,c) = a!(r,c) == '#'

count p = filter p >>> length
