#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array

-- Alternative solution to #10 part 2, using Pick's Theorem
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Control.Category ((>>>))
import Data.Array.Unboxed
import Data.Function (on)
import Data.List (find, groupBy)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

main =
  interact
    $ readInput
    >>> applyAll [solveA, solveB]
    >>> map show
    >>> unlines

type Input = UArray Coord Char

data Dir = N | E | S | W deriving (Eq, Ord, Show, Enum, Bounded)
dirs :: [Dir]
dirs = [N .. W]
go :: Dir -> Coord -> Coord
go = \case
  N -> above
  E -> right
  S -> below
  W -> left
rev :: Dir -> Dir
rev = \case
  N -> S
  S -> N
  E -> W
  W -> E

ends :: Char -> [Dir]
ends = \case
  '|' -> [N, S]
  '-' -> [E, W]
  'L' -> [N, E]
  'J' -> [N, W]
  '7' -> [S, W]
  'F' -> [S, E]
  'S' -> [N, S, E, W]
  _ -> []

readInput :: String -> Input
readInput = lines >>> mkArray

start :: Input -> Coord
start = assocs >>> find' (snd >>> (== 'S')) >>> fst

type Output = Int

-- Custom DFS to find the sequence of vertices along the loop
findLoop :: Input -> [Coord]
findLoop grid = loop
 where
  s = start grid
  s2 = head (nexts s)
  nexts c = filter (connected grid c) (neighborsIn grid c)

  loop = s : s2 : takeWhile (/= s) (zipWith next loop (tail loop))
  next l1 l2 = head (filter (/= l1) (nexts l2))

connected :: UArray Coord Char -> Coord -> Coord -> Bool
connected grid l1 l2 = any (\d -> go d l1 == l2 && d `elem` ends (grid ! l1) && rev d `elem` ends (grid ! l2)) dirs

-- Shoelace formula to compute area
area2 :: [Coord] -> Int
area2 vs = abs . sum $ zipWith cross vs (tail vs ++ [head vs])
 where
  cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

-- Count interior points via Pick's theorem
insideCount :: [Coord] -> Int
insideCount vs = (area2 vs - length vs) `div` 2 + 1

solveA, solveB :: Input -> Output
solveA = findLoop >>> length >>> (`div` 2)
solveB = findLoop >>> insideCount

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

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
