#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Category ((>>>))
import Data.Array.Unboxed
import Data.Set (Set)
import Data.Set qualified as S

------------------------------------------------------------
-- Input

type Input = UArray Coord Char

readInput :: String -> Input
readInput = lines >>> mkArray

------------------------------------------------------------
-- Solution

data Beam = Beam {beamLoc :: Coord, beamDir :: Vec}
  deriving (Eq, Ord, Show)

initBeam :: Beam
initBeam = Beam (0, 0) (0, 1)

energized :: Input -> Beam -> Int
energized grid initBeam =
  bfs (const False) next (S.singleton initBeam) >$> S.unions >>> S.map beamLoc >>> S.size
 where
  nextDir :: Beam -> [Vec]
  nextDir (Beam loc dir) =
    case grid ! loc of
      '.' -> [dir]
      '/'
        | fst dir == 0 -> [turnLeft dir]
        | otherwise -> [turnRight dir]
      '\\'
        | fst dir == 0 -> [turnRight dir]
        | otherwise -> [turnLeft dir]
      '-'
        | fst dir == 0 -> [dir]
        | otherwise -> [turnLeft dir, turnRight dir]
      '|'
        | fst dir == 0 -> [turnLeft dir, turnRight dir]
        | otherwise -> [dir]

  next :: Beam -> Set Beam
  next b@(Beam loc dir) = b >$> nextDir >>> map (\d -> Beam (loc ^+^ d) d) >>> filter (beamLoc >>> inRange (bounds grid)) >>> S.fromList

startingBeams :: Input -> [Beam]
startingBeams grid =
  concat $
    [[Beam (r, 0) (0, 1), Beam (r, cmax) (0, -1)] | r <- [0 .. rmax]]
      ++ [[Beam (0, c) (1, 0), Beam (rmax, c) (-1, 0)] | c <- [0 .. cmax]]
 where
  (_, (rmax, cmax)) = bounds grid

type Output = Int

solveA, solveB :: Input -> Output
solveA grid = energized grid initBeam
solveB grid = startingBeams grid >$> map (energized grid) >>> maximum

------------------------------------------------------------
-- Main

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

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

turnLeft, turnRight :: Vec -> Vec
turnLeft (r, c) = (-c, r)
turnRight (r, c) = (c, -r)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows)
