import           Control.Arrow
import           Data.List (tails)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = lines >>> map read

type Output = Int

solveA, solveB :: Input -> Output
solveA = (zipWith (<) <*> tail) >>> count id
solveB = tails >>> map (take 3) >>> takeWhile ((==3).length) >>> map sum
  >>> solveA

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
