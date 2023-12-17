#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package array --package search-algorithms

{-# LANGUAGE FlexibleContexts #-}

import Algorithm.Search
import Control.Category ((>>>))
import Data.Array.Unboxed
import Data.Maybe (fromJust)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = UArray Coord Int

readInput :: String -> Input
readInput = lines >>> map (map ((: []) >>> read)) >>> mkArray

type Output = Int

solve :: Int -> Int -> Node -> Input -> Output
solve minStraight maxStraight start grid =
  dijkstra (next grid minStraight maxStraight) (cost grid) (isGoal grid minStraight) start >$> fromJust >>> fst

solveA, solveB :: Input -> Output
solveA = solve 1 3 initNode
solveB = solve 4 10 initNode

-- If we are allowed to take the first step to the south:
-- min (solve 4 10 initNode grid) (solve 4 10 (Node (0, 0) (1, 0) 0) grid)

------------------------------------------------------------

data Node = Node {loc :: Coord, dir :: Vec, straightCount :: Int}
  deriving (Eq, Ord, Show)

initNode = Node (0, 0) (0, 1) 0

next :: Input -> Int -> Int -> Node -> [Node]
next grid minStraight maxStraight (Node l d c) =
  filter (loc >>> inRange (bounds grid)) $
    [Node (l ^+^ d') d' 1 | c >= minStraight, d' <- [turnLeft d, turnRight d]]
      ++ [Node (l ^+^ d) d (c + 1) | c < maxStraight]

cost :: Input -> Node -> Node -> Int
cost grid _ (Node l _ _) = grid ! l

isGoal :: Input -> Int -> Node -> Bool
isGoal grid ms (Node l _ c) = l == snd (bounds grid) && c >= ms

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int, Int)
type Vec = Coord

(^+^) :: Vec -> Vec -> Vec
(r1, c1) ^+^ (r2, c2) = (r1 + r2, c1 + c2)

turnLeft, turnRight :: Vec -> Vec
turnLeft (r, c) = (-c, r)
turnRight (r, c) = (c, -r)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows)
