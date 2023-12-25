#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Arrow ((***), (>>>))
import Data.Array.Unboxed
import Data.List (find, transpose)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB 202300]
      >>> map show
      >>> unlines

type Input = UArray Coord Char

readInput :: String -> Input
readInput = lines >>> mkArray

type Output = Int

gridBFS :: Bool -> Coord -> Input -> [Int]
gridBFS wrap start grid = layers >$> map S.size
 where
  okNeighbors = filter (((`mod` 131) *** (`mod` 131)) >>> (grid !) >>> (/= '#')) >>> S.fromList
  next
    | wrap = neighbors >>> okNeighbors
    | otherwise = neighborsIn grid >>> okNeighbors
  layers = bfs (const False) next (S.singleton start)

reachable' :: Bool -> Int -> Coord -> Input -> Int
reachable' wrap steps start = gridBFS wrap start >>> take (steps + 1) >>> chunksOf 2 >>> transpose >>> (!! (steps `mod` 2)) >>> sum

reachable :: Int -> Coord -> Input -> Int
reachable = reachable' False

solveA :: Input -> Output
solveA grid = reachable 64 (startLoc grid) grid

startLoc :: Input -> Coord
startLoc = assocs >>> find' (snd >>> (== 'S')) >>> fst

------------------------------------------------------------

-- Input has special properties: (1) there is a clear strip all the
-- way around the edge, and (2) there is a clear path straight from
-- the start location to every edge.  This means that although
-- progress may be impeded a bit within a tile, we can still predict
-- exactly what the shortest path is to all cells on the boundary of
-- every tile (it is exactly equal to the Manhattan distance from the
-- start).  Hence, for a given tile we know exactly what the first
-- cell reached will be: either one of the four sides, if it's a tile
-- directly in line with the start, or a corner otherwise.  So all we
-- need to do is (1) count how many odd cells can be reached inside an
-- entire tile, and how many even cells (the tiles will alternate all
-- odd cells / all even cells in a checkerboard pattern); (2) use mod
-- to figure out how many complete tiles we will have filled after
-- 26501365 steps (how many even + how many odd) (3) simulate filling
-- a tile from four sides + corners to distance given by the
-- remainder, and count how many we have of each on the boundary.

-- After 131s steps we have exactly reached the start points of new
-- tiles in the cardinal directions.  At that point we have
--   (1) 1 partial N/S/E/W tile each
--   (2) (s-1) partial NW/NE/SW/SE tiles each
--   (3) s^2 complete odd tiles, (s-1)^2 complete even tiles

-- 1, 5, 13, 25, ... complete tiles; s^2 odd and (s-1)^2 even tiles
--
-- X     -- 1 odd
--
--  X
-- XXX   -- 4 odd, 1 even
--  X
--
--   X
--  XXX
-- XXXXX  -- 9 odd, 4 even
--  XXX
--   X
--
-- In fact, the number of steps we want is 26501365 == 202300 * 131 + 65.
-- After that many steps we will have reached exactly the border of the tiles
-- in the cardinal directions.
--
-- After 131s + 65 steps, we have:
--   (1) 1 partial N/S/E/W tile each, at 131 steps
--   (2) (s-1) partial diagonal tiles each at (131 + 65) steps
--   (3) s partial diagonal tiles each at 65 steps
--   (4) (s-1)^2 complete odd tiles, s^2 complete even tiles

solveB :: Int -> Input -> Output
solveB s grid = sum $ sides ++ map ((s - 1) *) diagBig ++ map (s *) diagSmall ++ [s ^ 2 * completeE, (s - 1) ^ 2 * completeO]
 where
  completeE = reachable 1000 (startLoc grid) grid
  completeO = reachable 1001 (startLoc grid) grid
  sides = [reachable 130 s grid | s <- [(0, 65), (65, 0), (65, 130), (130, 65)]]
  corners = [(0, 0), (130, 0), (0, 130), (130, 130)]
  diagBig = [reachable (130 + 65) s grid | s <- corners]
  diagSmall = [reachable 64 s grid | s <- corners]

solveBNaive :: Int -> Input -> Output
solveBNaive s grid = reachable' True (s * 131 + 65) (startLoc grid) grid

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> Set a) -> Set a -> [Set a]
bfs isGoal next = bfs' S.empty
 where
  bfs' seen layer
    | S.null layer = []
    | any isGoal layer = [layer]
    | otherwise = layer : bfs' seen' layer'
   where
    layer' = foldMap next layer `S.difference` seen'
    seen' = S.union seen layer

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int, Int)

above, below, left, right :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows)

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, left, right]

neighborsIn :: IArray UArray a => UArray Coord a -> Coord -> [Coord]
neighborsIn a = filter (inRange (bounds a)) . neighbors
