#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow   ((>>>))
import           Data.List       (transpose)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Int]]

readInput :: String -> Input
readInput = lines >>> map (map ((:[]) >>> read))

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA = withIndices >>> orientations >>> map (concatMap recordMaxima >>> S.fromList) >>> S.unions >>> S.size
solveB = withIndices >>> orientations >>> map (concatMap dists >>> M.fromList) >>> M.unionsWith (*) >>> M.elems >>> maximum

withIndices :: [[a]] -> [[(a, (Int,Int))]]
withIndices xs = [[(x, (i,j)) | (x,j) <- zip row [0 ..]] | (row,i) <- zip xs [0 ..]]

orientations :: [[a]] -> [[[a]]]
orientations = applyAll [id, map reverse, transpose, map reverse . transpose]

recordMaxima :: [(Int, ij)] -> [ij]
recordMaxima [] = []
recordMaxima ((x, ij):xs) = ij : go x xs
  where
    go _ [] = []
    go m ((x, ij):xs)
      | m < x     = ij : go x xs
      | otherwise = go m xs

dists :: [(Int, ij)] -> [(ij, Int)]
dists = go [(100,0)] 0
  where
    go _ _ [] = []
    go ((y,i):stk) k ((x, ij):xs)
      | x <= y = (ij, k-i) : go ((x,k) : (y,i) : stk) (k+1) xs
      | otherwise = go stk k ((x, ij):xs)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
