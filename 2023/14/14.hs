#!/usr/bin/env stack
-- stack --resolver lts-19.28 script

import Control.Arrow (second, (***), (>>>))
import Data.List (elemIndex, find, transpose)
import Data.Maybe (fromJust)

------------------------------------------------------------
-- Input

type Grid = [[Char]]
type Input = Grid

readInput :: String -> Input
readInput = lines >>> transpose

-- For debugging
drawGrid :: Grid -> IO ()
drawGrid = transpose >>> unlines >>> putStr

------------------------------------------------------------
-- Solution

rollCol :: [Char] -> [Char]
rollCol xs = zipWith (\x n -> if n == 1 then 'O' else x) cleared smeared
 where
  cleared = map (\x -> if x == '#' then x else '.') xs
  counts = scanr (\x n -> case x of 'O' -> n + 1; '.' -> n; '#' -> 0) 0 xs
  collected = zipWith (\x n -> if x == '#' then n else 0) ('#' : xs) counts
  smeared = zipSum (map (`replicate` 1) collected)

roll, spin, tilt, cyc :: Grid -> Grid
roll = map rollCol
spin = transpose >>> reverse
tilt = roll >>> spin
cyc = tilt >>> tilt >>> tilt >>> tilt

load :: Grid -> Int
load = map columnLoad >>> sum
 where
  columnLoad = reverse >>> zipWith (\r x -> if x == 'O' then r else 0) [1 ..] >>> sum

------------------------------------------------------------
-- Main

type Output = Int

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

solveA, solveB :: Input -> Output
solveA = roll >>> load
solveB grid = load (iterate cyc grid !! (μ + n))
 where
  (μ, λ) = floyd cyc grid
  n = (1000000000 - μ) `mod` λ

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

zipWithL _ [] ys = ys
zipWithL _ xs [] = xs
zipWithL f (x : xs) (y : ys) = f x y : zipWithL f xs ys

zipSum :: [[Int]] -> [Int]
zipSum [] = []
zipSum ([] : xss) = 0 : zipSum xss
zipSum ((x : xs) : xss) = x : zipWithL (+) xs (zipSum xss)

-- Floyd's cycle-finding algorithm.  floyd f x0 returns the smallest
-- (μ, λ) such that (s !! (μ + i) == s !! (μ + i + λ)) for all i >= 0
-- where s = iterate f x0.
floyd :: Eq a => (a -> a) -> a -> (Int, Int)
floyd f x0 = (μ, λ)
 where
  tortoiseHare = f *** (f . f)
  step = f *** f

  findMatch :: Eq a => ((a, a) -> (a, a)) -> (a, a) -> (Int, a)
  findMatch g = iterate g >>> zip [0 ..] >>> find' (snd >>> uncurry (==)) >>> second fst

  (_, xν) = findMatch tortoiseHare (tortoiseHare (x0, x0))
  (μ, xμ) = findMatch step (x0, xν)
  λ = succ . fromJust $ elemIndex xμ (iterate f (f xμ))
