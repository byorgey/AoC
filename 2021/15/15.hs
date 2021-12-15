#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Algorithm.Search   (dijkstra)
import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (UArray, bounds, inRange, listArray, (!))
import           Data.Maybe         (fromJust)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Int]]

readInput :: String -> Input
readInput = lines >>> map (map ((:[]) >>> read))

------------------------------------------------------------

mkArray :: [[Int]] -> UArray (Int,Int) Int
mkArray a = listArray ((1,1),(r,c)) (concat a)
  where
    r = length a
    c = length (head a)

mkArray5 :: [[Int]] -> UArray (Int,Int) Int
mkArray5 a = mkArray g
  where
    inca = map (map inc)
    inc 9 = 1
    inc x = x+1

    r = foldr (zipWith (++)) (repeat []) (take 5 (iterate inca a))
    g = concat (take 5 (iterate inca r))

type Output = Int

solveA, solveB :: Input -> Output
solveA = mkArray >>> solve
solveB = mkArray5 >>> solve

solve :: UArray (Int,Int) Int -> Int
solve a = dijkstra next (\_ v -> a!v) (==snd (bounds a)) (1,1) >$> fromJust >>> fst
  where
    next (r,c) = filter (inRange (bounds a)) [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
