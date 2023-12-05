#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package split

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Category ((>>>))
import Data.Foldable qualified as F
import Data.List (find)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, fromMaybe)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

------------------------------------------------------------
-- Input

type Input = Almanac
data Almanac = Almanac {seeds :: [Int], maps :: [ConvMap]} deriving (Show)
data ConvMap = CM {source :: String, dest :: String, mapping :: [RangeMap]} deriving (Show)
data RangeMap = RM {destStart :: Int, sourceStart :: Int, rangeLen :: Int} deriving (Show)

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> readAlmanac

readAlmanac :: [[String]] -> Almanac
readAlmanac ([ss] : ms) = Almanac (ss >$> words >>> drop 1 >>> map read) (map readMap ms)

readMap :: [String] -> ConvMap
readMap (title : rs) = CM s d (map readRangeMap rs)
 where
  [s, _, d] = splitOn "-" (head (words title))
  readRangeMap = words >>> map read >>> (\[a, b, c] -> RM a b c)

seedRanges :: [Int] -> [Interval]
seedRanges = chunksOf 2 >>> map (\[x, l] -> I x (x + l - 1))

------------------------------------------------------------
-- Lookup + inversion

lookupCM :: ConvMap -> Int -> Int
lookupCM (CM _ _ rs) i = fromMaybe i (F.asum (map (`lookupRM` i) rs))

lookupRM :: RangeMap -> Int -> Maybe Int
lookupRM (RM d s l) i
  | i >= s && i < s + l = Just $ d + (i - s)
  | otherwise = Nothing

follow :: [ConvMap] -> Int -> Int
follow = map lookupCM >>> foldr (>>>) id

invRM :: RangeMap -> RangeMap
invRM (RM d s l) = RM s d l

invCM :: ConvMap -> ConvMap
invCM (CM s d rs) = CM d s (map invRM rs)

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA Almanac {..} = minimum $ map (follow maps) seeds
solveB Almanac {..} = find' (follow (map invCM $ reverse maps) >>> \s -> any (s ∈) ss) [1 ..]
 where
  ss = seedRanges seeds

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)

data Interval = I {lo :: Int, hi :: Int} deriving (Eq, Ord, Show)

(∈) :: Int -> Interval -> Bool
x ∈ I l h = l <= x && x <= h
