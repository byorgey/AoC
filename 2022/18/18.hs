#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow   ((&&&), (***), (>>>))
import           Data.List       (find, transpose)
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Coord3 = [Int]
type Input = Set Coord3

readInput :: String -> Input
readInput = lines >>> map (splitOn "," >>> map read) >>> S.fromList

------------------------------------------------------------

neighbors :: Coord3 -> [Coord3]
neighbors [x,y,z] = [[x+1,y,z], [x-1,y,z], [x,y+1,z], [x,y-1,z], [x,y,z+1], [x,y,z-1]]

surfaceArea :: Set Coord3 -> Int
surfaceArea s = s >$> S.toList >>> map (neighbors >>> count (`S.notMember` s)) >>> sum

externalSurfaceArea :: Set Coord3 -> Int
externalSurfaceArea s = surfaceArea outside - cubeSurfaceArea (map (uncurry subtract >>> succ) bds)
  where
    bds = s >$> S.toList >>> transpose >>> map ((minimum >>> pred) &&& (maximum >>> succ))
    outside = S.unions $ bfs (const False) next (S.singleton (map fst bds))
    next = neighbors >>> filter (\c -> (c `S.notMember` s) && inBounds c) >>> S.fromList
    cubeSurfaceArea [x,y,z] = 2*(x*y + x*z + y*z)

    inBounds = and . zipWith inRange bds
    inRange (l,h) a = l <= a && a <= h

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA = surfaceArea
solveB = externalSurfaceArea

------------------------------------------------------------

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | S.null layer     = []
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = foldMap next layer `S.difference` seen'
          seen' = S.union seen layer

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
