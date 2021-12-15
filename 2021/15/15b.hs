#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Algorithm.Search   (dijkstra)
import           Control.Arrow      ((>>>), (***))
import           Data.Array.Unboxed (UArray, bounds, inRange, listArray, (!))
import           Data.Maybe         (fromJust)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = UArray (Int,Int) Int

readInput :: String -> Input
readInput = lines >>> map (map ((:[]) >>> read)) >>> mkArray

------------------------------------------------------------

mkArray :: [[Int]] -> UArray (Int,Int) Int
mkArray a = listArray ((0,0),(r-1,c-1)) (concat a)
  where
    r = length a
    c = length (head a)

type Output = Int

solveA, solveB :: Input -> Output
solveA = solve 1
solveB = solve 5

solve :: Int -> UArray (Int,Int) Int -> Int
solve k a = dijkstra next (\_ v -> cost v) (==snd bds) (0,0) >$> fromJust >>> fst
  where
    (rows,cols) = (succ *** succ) (snd (bounds a))
    bds = ((0,0), (k*rows - 1, k*cols - 1))
    next (r,c) = filter (inRange bds) [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]

    cost (r,c) = (a!(r `mod` rows, c `mod` cols) + r `div` rows + c `div` cols - 1) `mod` 9 + 1

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
