#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package split --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Category ((>>>))
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (second)
import Data.List (intercalate)
import Data.List.Split (splitOn, wordsBy)

------------------------------------------------------------
-- Read input

main =
  interact
    $ readInput
    >>> applyAll [solveA, solveB]
    >>> map show
    >>> unlines

type Input = [(String, [Int])]

readInput :: String -> Input
readInput = lines >>> map (words >>> toPair >>> second (splitOn "," >>> map read))

------------------------------------------------------------
-- Brute force solution
-- Good enough for Part 1, and useful for testing

allSubs :: String -> [String]
allSubs [] = [[]]
allSubs ('?' : s) = let ss = allSubs s in map ('.' :) ss ++ map ('#' :) ss
allSubs (x : s) = map (x :) (allSubs s)

matches :: [Int] -> String -> Bool
matches xs = wordsBy (== '.') >>> map length >>> (== xs)

arrangementsBrute :: String -> [Int] -> Int
arrangementsBrute s ns = count (matches ns) (allSubs s)

------------------------------------------------------------
-- DP solution

matchOne :: Char -> Char -> Bool
matchOne x y = (x == '?') || (y == '?') || (x == y)

arrangements :: String -> [Int] -> Int
arrangements springs blocks = n (length springs, length blocks)
 where
  s :: UArray Int Char
  s = listArray (0, length springs - 1) springs

  d :: UArray Int Int
  d = listArray (0, length blocks - 1) blocks

  matchesBlock start end =
    and
      [ start >= 0
      , all (\i -> matchOne (s ! i) '#') [start .. end - 1]
      , start == 0 || matchOne (s ! (start - 1)) '.'
      ]

  n :: (Int, Int) -> Int
  n = memo ((0, 0), (length springs, length blocks)) $ \case
    (i, _) | i < 0 -> 0
    (i, 0) -> fromEnum ('#' `notElem` take i springs)
    (0, k) -> fromEnum (k == 0)
    (i, k) ->
      let p = i - d ! (k - 1)
       in (if matchesBlock p i then (if p == 0 then fromEnum (k == 1) else n (p - 1, k - 1)) else 0)
            + (if matchOne (s ! (i - 1)) '.' then n (i - 1, k) else 0)

------------------------------------------------------------
-- Main

unfold :: (String, [Int]) -> (String, [Int])
unfold (ss, bs) = (intercalate "?" (replicate 5 ss), concat (replicate 5 bs))

solveA, solveB :: Input -> Int
solveA = map (uncurry arrangements) >>> sum
solveB = map (unfold >>> uncurry arrangements) >>> sum

------------------------------------------------------------
-- Utilities

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

tabulate :: Ix i => (i, i) -> (i -> e) -> Array i e
tabulate rng f = listArray rng (map f $ range rng)

memo :: Ix i => (i, i) -> (i -> a) -> (i -> a)
memo rng = (!) . tabulate rng
