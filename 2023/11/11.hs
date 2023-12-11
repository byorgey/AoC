#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Category ((>>>))
import Data.Array.Unboxed
import Data.Bool (bool)
import Data.Set (Set)
import Data.Set qualified as S

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = [[Bool]]

readInput :: String -> Input
readInput = lines >>> map (map (== '#'))

-- Naive expansion for Part 1
-- expand, expandRows :: Input -> Input
-- expand = expandRows >>> transpose >>> expandRows >>> transpose
-- expandRows = concatMap (\r -> if all not r then [r, r] else [r])

expand :: Int -> [Coord] -> [Coord]
expand k gs = map expandCoords gs
 where
  galaxyRows = S.fromList $ map fst gs
  galaxyCols = S.fromList $ map snd gs

  blankRows = S.fromList [0 .. S.findMax galaxyRows] `S.difference` galaxyRows
  blankCols = S.fromList [0 .. S.findMax galaxyCols] `S.difference` galaxyCols

  expandCoords (r, c) =
    ( r + (k - 1) * S.size (fst (S.split r blankRows))
    , c + (k - 1) * S.size (fst (S.split c blankCols))
    )

printGrid :: Input -> IO ()
printGrid = map (map (bool '.' '#')) >>> unlines >>> putStr

galaxies :: Input -> [Coord]
galaxies = mkArray >>> assocs >>> filter snd >>> map fst

type Output = Int

solve :: Int -> Input -> Output
solve k = galaxies >>> expand k >>> pairs >>> map (uncurry manhattan) >>> sum

solveA, solveB :: Input -> Output
solveA = solve 2
solveB = solve 1000000

------------------------------------------------------------
-- Utilities

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Coord = (Int, Int)

manhattan :: Coord -> Coord -> Int
manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows)
