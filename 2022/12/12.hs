#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array

{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (IArray (..), Ix (inRange), UArray, assocs,
                                     listArray, (!))
import           Data.Char          (ord)
import           Data.List          (find)
import           Data.Maybe         (fromJust)
import           Data.Set           (Set)
import qualified Data.Set           as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Coord = (Int,Int)
type Input = UArray Coord Char

readInput :: String -> Input
readInput = lines >>> mkArray

mkArray :: IArray UArray a => [[a]] -> UArray (Int,Int) a
mkArray rows = listArray ((0,0), (length rows - 1, length (head rows) - 1)) (concat rows)

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA a = pred . length $ bfs ((=='E') . (a!)) (next fwd a) (S.singleton $ findCell 'S' a)
solveB a = pred . length $ bfs ((`elem` "aS") . (a!)) (next (flip fwd) a) (S.singleton $ findCell 'E' a)

next :: (Char -> Char -> Bool) -> UArray Coord Char -> Coord -> Set Coord
next allowed a cur@(r,c) = S.fromList $ filter (\n -> inRange (bounds a) n && ok n) [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]
  where
    ok i = allowed (toHt (a!cur)) (toHt (a!i))
    toHt 'S' = 'a'
    toHt 'E' = 'z'
    toHt x   = x

fwd x y = ord y <= ord x + 1

findCell :: Char -> UArray Coord Char -> Coord
findCell c = assocs >>> find' (snd >>> (==c)) >>> fst

------------------------------------------------------------

bfs :: Ord a => (a -> Bool) -> (a -> Set a) -> Set a -> [Set a]
bfs isGoal next = bfs' S.empty
  where
    bfs' seen layer
      | S.null layer     = []
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = foldMap next layer `S.difference` seen'
          seen' = S.union seen layer

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust
