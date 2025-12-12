#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package split --package array

import Control.Arrow ((>>>))
import Data.Array.Unboxed
import Data.List.Split

------------------------------------------------------------
-- Data types + input parsing

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Present = UArray (Int, Int) Bool
data Tree = Tree {treeSize :: (Int, Int), presentCounts :: [Int]}
  deriving (Show)
data Input = Input {presents :: [Present], trees :: [Tree]}
  deriving (Show)

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> (\gs -> Input (map readPresent (init gs)) (map readTree (last gs)))

readPresent :: [String] -> Present
readPresent = drop 1 >>> map (map (== '#')) >>> mkArray

readTree :: String -> Tree
readTree = words >>> (\(sz : cs) -> Tree (readSize sz) (map read cs))

readSize :: String -> (Int, Int)
readSize = init >>> splitOn "x" >>> map read >>> toPair

------------------------------------------------------------

presentArea :: Present -> Int
presentArea = elems >>> count id

excessArea :: [Int] -> Tree -> Int
excessArea presentAreas (Tree (r, c) cs) = r * c - sum (zipWith (*) cs presentAreas)

type Output = Int

solveA, solveB :: Input -> Output
solveA (Input ps ts) = count (\t -> excessArea presentAreas t >= 0) ts
 where
  presentAreas = map presentArea ps
solveB = const 0

------------------------------------------------------------
-- Utilities

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

type Coord = (Int, Int)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows@(r : _) = listArray ((0, 0), (length rows - 1, length r - 1)) (concat rows)
