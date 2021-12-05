{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Arrow
import           Data.Array.Unboxed
import           Data.List
import           Data.List.Split

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type P2 = [Int]
type Line = [P2]
type Input = [Line]

readInput :: String -> Input
readInput = lines >>> map (words >>> parsePoints)
  where
    parsePoints [p1,_,p2] = map (splitOn "," >>> map read) [p1,p2]

type Output = Int

solveA, solveB :: Input -> Output
solveA = solve False
solveB = solve True

solve :: Bool -> Input -> Output
solve includeDiags ls =
  ls >$> concatMap (expandLine includeDiags) >>> map (\[x,y] -> ((x,y),1)) >>>
  accumArray @UArray @Int @(Int,Int) (+) 0 ((0,0), (xmax,ymax)) >>>
  elems >>> count (>= 2)
  where
    [xmax,ymax] = ls >$> concat >>> transpose >>> map maximum

------------------------------------------------------------

expandLine :: Bool -> Line -> [P2]
expandLine includeDiags [[x1,y1], [x2,y2]]
  | x1 == x2 = [[x1,y] | y <- enum y1 y2]
  | y1 == y2 = [[x,y1] | x <- enum x1 x2]
  | includeDiags = [[x,y] | x <- enum x1 x2 | y <- enum y1 y2]
  | otherwise = []

enum :: Int -> Int -> [Int]
enum a b
  | a <= b = [a .. b]
  | otherwise = [a, a-1 .. b]

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)


