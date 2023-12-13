#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package split --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Category ((>>>))
import Data.Array.Unboxed
import Data.List.Split (splitOn)
import Data.Tuple (swap)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Pattern = UArray Coord Bool
type Input = [Pattern]

readInput :: String -> Input
readInput = lines >>> map (map (== '#')) >>> splitOn [[]] >>> map mkArray

transposePattern :: Pattern -> Pattern
transposePattern a = array ((0, 0), swap lr) [(swap c, a ! c) | c <- range bds]
 where
  bds@(_, lr) = bounds a

-- Check whether there is a reflection at a particular row
isReflection :: Pattern -> Int -> Bool
isReflection p rr = all (\r -> rowsMatch r (2 * rr - 1 - r)) [0 .. rr - 1]
 where
  (_, (rmax, cmax)) = bounds p
  rowsMatch i j = j > rmax || all (\c -> p ! (i, c) == p ! (j, c)) [0 .. cmax]

rowReflections :: Pattern -> [Int]
rowReflections p = filter (isReflection p) [1 .. rmax]
 where
  (_, (rmax, _)) = bounds p

data Reflection = Row Int | Col Int deriving (Eq, Ord, Show)

reflectionCode :: Reflection -> Int
reflectionCode = \case
  Row n -> 100 * n
  Col n -> n

reflections :: Pattern -> [Reflection]
reflections p = map Row (rowReflections p) ++ map Col (rowReflections (transposePattern p))

reflectionNum :: Pattern -> Int
reflectionNum = reflections >>> map reflectionCode >>> sum

-- Try changing each cell of a pattern
experiment :: Pattern -> [Pattern]
experiment p = [modify c | c <- range (bounds p)]
 where
  modify i = array (bounds p) [(j, (if i == j then not else id) (p ! j)) | j <- range (bounds p)]

newReflectionNum :: Pattern -> Int
newReflectionNum p = p >$> experiment >>> concatMap reflections >>> filter (/= old) >>> head >>> reflectionCode
 where
  old = head (reflections p)

type Output = Int

solveA, solveB :: Input -> Output
solveA = map reflectionNum >>> sum
solveB = map newReflectionNum >>> sum

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int, Int)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows)
