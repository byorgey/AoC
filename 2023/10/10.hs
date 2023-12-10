#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array

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
  interact $
    readInput
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

findLoop :: Input -> [Set Coord]
findLoop grid = bfs (const False) next (S.singleton $ start grid)
 where
  next c = S.fromList $ filter (connected grid c) (neighborsIn grid c)

connected :: UArray Coord Char -> Coord -> Coord -> Bool
connected grid l1 l2 = any (\d -> go d l1 == l2 && d `elem` ends (grid ! l1) && rev d `elem` ends (grid ! l2)) dirs

inside :: UArray Coord Char -> Set Coord
inside grid = range bds >$> filter (`S.notMember` loop) >>> filter isInside >>> S.fromList
 where
  s = start grid
  startIsCrossing = any (all (\d -> connected grid s (go d s))) [[N, S], [S, E], [S, W]]
  crossings
    | startIsCrossing = "|F7S"
    | otherwise = "|F7"
  isCrossing l = l `S.member` loop && (grid ! l) `elem` crossings
  bds = bounds grid
  loop = S.unions (findLoop grid)
  isInside = iterate right >>> takeWhile (inRange bds) >>> count isCrossing >>> odd

solveA, solveB :: Input -> Output
solveA = findLoop >>> length >>> pred
solveB = inside >>> length

drawLoop :: UArray Coord Char -> Set Coord -> Set Coord -> [[Char]]
drawLoop grid loop ins =
  assocs grid
    >$> map drawChar
    >>> groupBy ((==) `on` (fst . fst))
    >>> map (map snd)
 where
  drawChar (l, x)
    | l `S.member` loop = (l, x)
    | l `S.member` ins = (l, '*')
    | otherwise = (l, ' ')

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next = bfs' S.empty
 where
  bfs' seen layer
    | S.null layer = []
    | any isGoal layer = [layer]
    | otherwise = layer : bfs' seen' layer'
   where
    layer' = foldMap next layer `S.difference` seen'
    seen' = S.union seen layer

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

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
