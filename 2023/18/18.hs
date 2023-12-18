#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow (second, (&&&), (>>>))
import Data.Array.Unboxed
import Data.Function (on)
import Data.List (elemIndex, groupBy, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Numeric qualified as N

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

data Dir = R | D | L | U deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Direction = Direction {dir :: Dir, dist :: Int, color :: String}
type Input = [Direction]

readInput :: String -> Input
readInput = lines >>> map readDirection

readDirection :: String -> Direction
readDirection = words >>> \[d, n, c] -> Direction (read d) (read n) (init . drop 2 $ c)

type Output = Int

solveA, solveB :: Input -> Output
solveA input = let t = trench input in length t + S.size (flood t)
solveB input = let ds = map convertDirection input in perimeter ds + insideCount ds

------------------------------------------------------------

applyDir :: Dir -> (Coord -> Coord)
applyDir = \case
  R -> right
  L -> left
  U -> above
  D -> below

dirToVec :: Dir -> Vec
dirToVec = \case
  R -> (0, 1)
  L -> (0, -1)
  U -> (-1, 0)
  D -> (1, 0)

------------------------------------------------------------
-- Part 1

trench :: Input -> [Coord]
trench = go (0, 0)
 where
  go _ [] = []
  go cur (Direction d n _ : ds) = next ++ go (last next) ds
   where
    (_ : next) = take (n + 1) (iterate (applyDir d) cur)

drawTrench :: [Coord] -> String
drawTrench locs = grid >$> assocs >>> groupBy ((==) `on` (fst >>> fst)) >>> map (map snd) >>> unlines
 where
  (rs, cs) = unzip locs
  (rmin, rmax) = (minimum &&& maximum) rs
  (cmin, cmax) = (minimum &&& maximum) cs

  grid :: UArray (Int, Int) Char
  grid =
    array ((rmin, cmin), (rmax, cmax)) $
      [((r, c), ' ') | r <- [rmin .. rmax], c <- [cmin .. cmax]]
        ++ [(loc, '#') | loc <- locs]

-- Find one point inside the trench
inside :: [Coord] -> Coord
inside locs = second (+ 1) (head secondRow)
 where
  rmin = locs >$> map fst >>> minimum
  secondRow = locs >$> filter (fst >>> (== rmin + 1)) >>> sortBy (comparing snd)

-- Flood fill the interior of the trench
flood :: [Coord] -> Set Coord
flood ts = S.unions $ bfs (const False) next (S.singleton (inside ts))
 where
  trenchSet = S.fromList ts
  next = neighbors >>> filter (`S.notMember` trenchSet) >>> S.fromList

------------------------------------------------------------
-- Part 2

convertDirection :: Direction -> Direction
convertDirection (Direction d n c) =
  Direction (toEnum (read [last c])) (readBase 16 (['0' .. '9'] ++ ['a' .. 'f']) (take 5 c)) ""

readBase :: Int -> [Char] -> String -> Int
readBase b digits s =
  case N.readInt b (`elem` digits) (flip elemIndex digits >>> fromJust) s of
    ((a, _) : _) -> a

perimeter :: [Direction] -> Int
perimeter = map dist >>> sum

vertices :: [Direction] -> [Coord]
vertices = scanl (\loc (Direction d n _) -> loc ^+^ (n *^ dirToVec d)) (0, 0)

-- Shoelace formula to compute area
area2 :: [Coord] -> Int
area2 vs = abs . sum $ zipWith cross vs (tail vs ++ [head vs])
 where
  cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

-- Count interior points via Pick's theorem
insideCount :: [Direction] -> Int
insideCount ds = (area2 (vertices ds) - perimeter ds) `div` 2 + 1

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

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int, Int)
type Vec = Coord

(^+^) :: Vec -> Vec -> Vec
(r1, c1) ^+^ (r2, c2) = (r1 + r2, c1 + c2)

(*^) :: Int -> Vec -> Vec
k *^ (r, c) = (k * r, k * c)

above, below, left, right :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, left, right]
