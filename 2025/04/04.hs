#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

import Control.Arrow ((>>>))
import Data.Array.Unboxed
import Data.List (unfoldr)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Grid = UArray Coord Bool

readInput :: String -> Grid
readInput = lines >>> map (map (== '@')) >>> mkArray

type Output = Int

solveA, solveB :: Grid -> Output
solveA grid = count (isAccessible grid) (filter (grid !) $ range (bounds grid))
solveB grid = sum . map (\g -> count (isRemovable g) (range (bounds grid))) . unfoldr remove $ grid

isAccessible :: Grid -> Coord -> Bool
isAccessible grid = (< 4) . count (grid !) . inGrid grid . neighbors8

isRemovable :: Grid -> Coord -> Bool
isRemovable grid i = grid ! i && isAccessible grid i

remove :: Grid -> Maybe (Grid, Grid)
remove grid
  | c > 0 = Just (grid, iamap (\i a -> a && not (isAccessible grid i)) grid)
  | otherwise = Nothing
 where
  c = count (isRemovable grid) (range (bounds grid))

------------------------------------------------------------
-- Utilities

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Coord = (Int, Int)

above, below, left, right :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows@(r : _) = listArray ((0, 0), (length rows - 1, length r - 1)) (concat rows)

iamap :: (Ix i, IArray UArray a) => (i -> a -> a) -> UArray i a -> UArray i a
iamap f a = listArray (bounds a) (map (uncurry f) (assocs a))

neighbors8 :: Coord -> [Coord]
neighbors8 = applyAll [above, below, left, right, above . left, above . right, below . left, below . right]

inGrid :: IArray UArray a => UArray Coord a -> [Coord] -> [Coord]
inGrid = filter . inRange . bounds
