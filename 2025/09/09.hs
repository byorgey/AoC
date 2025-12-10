#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

import Control.Arrow ((>>>))
import Data.List.Split (chunksOf, splitOn)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = [V2]

readInput :: String -> Input
readInput = lines >>> map (splitOn "," >>> map read >>> toPair)

type Output = Int

solveA, solveB :: Input -> Output
solveA = rects >>> map area >>> maximum
solveB input = input >$> rects >>> filter (\r -> all (ok r) es) >>> map area >>> maximum
 where
  es = edges input
  -- A rectangle is OK if it lies fully to the right of every edge it
  -- has a nontrivial intersection with
  ok r e = intersects e r --> rightOf e r

------------------------------------------------------------
-- Rectangles

data Rect = Rect V2 V2

area :: Rect -> Int
area (Rect (x1, y1) (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

rects :: [V2] -> [Rect]
rects = pairs >>> map (uncurry Rect)

------------------------------------------------------------
-- Edges

data Edge = Edge V2 V2

-- Note edges are oriented clockwise around the shape in both sample +
-- input.
edges :: [V2] -> [Edge]
edges (v : vs) = zipWith Edge (v : vs) (vs ++ [v])

------------------------------------------------------------
-- 2D computational geometry

-- 2D cross product, aka signed area of the parallelogram formed by
-- the given vectors
cross :: V2 -> V2 -> Int
cross (ux, uy) (vx, vy) = ux * vy - vx * uy

crossP :: V2 -> V2 -> V2 -> Int
crossP p1 p2 p3 = cross (p2 ^-^ p1) (p3 ^-^ p1)

(^-^) :: V2 -> V2 -> V2
(x1, y1) ^-^ (x2, y2) = (x1 - x2, y1 - y2)

-- Test whether a rectangle lies entirely to the right of an
-- (oriented) edge.
rightOf :: Edge -> Rect -> Bool
rightOf (Edge p q) (Rect c1 c2) = crossP p q c1 >= 0 && crossP p q c2 >= 0

-- Test whether the first thing is in between the other two.
between :: Ord a => a -> a -> a -> Bool
between a x y = a >= min x y && a <= max x y

-- Test whether an edge intersects a rectangle (in more than a single corner).
intersects :: Edge -> Rect -> Bool
intersects (Edge (px, py) (qx, qy)) (Rect (cx, cy) (dx, dy))
  | px == qx = between px cx dx && max py qy > min cy dy && min py qy < max cy dy
  | otherwise = between py cy dy && max px qx > min cx dx && min px qx < max cx dx

------------------------------------------------------------
-- Utilities

type V2 = (Int, Int)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

infixr 0 >$>
(>$>) = flip ($)

infixr 1 -->
p --> q = not p || q
