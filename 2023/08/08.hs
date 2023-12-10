#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow ((&&&), (>>>))
import Data.Array.Unboxed
import Data.Bifunctor (second)
import Data.Bits (shiftR, testBit)
import Data.Char (isAlpha)
import Data.List (find, findIndex, genericLength)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

main =
    interact
        $ readInput
        >>> applyAll [solveA, solveB]
        >>> map show
        >>> unlines

type Node = String
data Input = Input {turnRight :: UArray Int Bool, network :: Map Node (Node, Node)} deriving (Show)

readInput :: String -> Input
readInput =
    lines >>> \(instrs : _ : nodes) ->
        Input (mkArray $ map (== 'R') instrs) (M.fromList $ map readNode nodes)

readNode :: String -> (Node, (Node, Node))
readNode = map keepAlpha >>> words >>> \[x, y, z] -> (x, (y, z))
  where
    keepAlpha c
        | isAlpha c = c
        | otherwise = ' '

turn :: Bool -> (a, a) -> a
turn = \case False -> fst; True -> snd

type Loc = (Node, Int)

nextLoc :: Input -> Loc -> Loc
nextLoc Input {..} (l, i) = (turn (turnRight ! i) (network !* l), (i + 1) `mod` n)
  where
    n = snd (bounds turnRight) + 1

follow :: Input -> Node -> [Loc]
follow i start = locs
  where
    locs = iterate (nextLoc i) (start, 0)

m !* k = case M.lookup k m of
    Just x -> x
    Nothing -> error $ "key error: " ++ show k ++ " map: " ++ show m

findCycle :: Ord a => (a -> a) -> a -> ([a], [a])
findCycle next start = break (== firstRepeat) as
  where
    (firstRepeat, as) = go S.empty start
    go !seen a
        | a `S.member` seen = (a, [])
        | otherwise = second (a :) (go (S.insert a seen) (next a))

------------------------------------------------------------
-- Modular exponentiation

modexp :: Integer -> Integer -> Integer -> Integer
modexp b e m = go e
  where
    go 0 = 1
    go e
        | e `testBit` 0 = (b * r * r) `mod` m
        | otherwise = (r * r) `mod` m
      where
        r = go (e `shiftR` 1)

------------------------------------------------------------
-- (Extended) Euclidean algorithm

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0
    | a < 0 = (-a, -1, 0)
    | otherwise = (a, 1, 0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g, x, y) = egcd b (a `mod` b)

-- g = bx + (a mod b)y
--   = bx + (a - b(a/b))y
--   = ay + b(x - (a/b)y)

-- inverse p a  is the multiplicative inverse of a mod p
inverse :: Integer -> Integer -> Integer
inverse p a = y `mod` p
  where
    (_, _, y) = egcd p a

------------------------------------------------------------
-- Solving modular equations

-- solveMod a b m  solves  ax = b (mod m), returning (y,k) such that all
-- solutions are equivalent to  y (mod k)
solveMod :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
solveMod a b m
    | g == 1 = Just ((b * inverse m a) `mod` m, m)
    | b `mod` g == 0 = solveMod (a `div` g) (b `div` g) (m `div` g)
    | otherwise = Nothing
  where
    g = gcd a m

-- gcrt solves a system of modular equations.  Each equation x = a
-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that
-- 0 <= z < k and solutions for x satisfy x = z (mod k), that is,
-- solutions are of the form x = z + kt for integer t.
gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt [e] = Just e
gcrt (e1 : e2 : es) = gcrt2 e1 e2 >>= \e -> gcrt (e : es)

-- gcrt2 (a,n) (b,m) solves the pair of modular equations
--
--   x = a (mod n)
--   x = b (mod m)
--
-- It returns a pair (c, k) such that 0 <= c < k and all solutions for
-- x satisfy x = c (mod k), that is, solutions are of the form x = c +
-- kt for integer t.
gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a, n) (b, m)
    | a `mod` g == b `mod` g = Just (((a * v * m + b * u * n) `div` g) `mod` k, k)
    | otherwise = Nothing
  where
    (g, u, v) = egcd n m
    k = (m * n) `div` g

------------------------------------------------------------
-- Solution

solveA, solveB :: Input -> Integer
solveA = flip follow "AAA" >>> map fst >>> takeWhile (/= "ZZZ") >>> length >>> fromIntegral
solveB input@(Input {..}) = let Just (c, k) = gcrt eqns in k + c
  where
    starts = network >$> M.keys >>> filter (last >>> (== 'A'))
    cycles = map ((,0) >>> findCycle (nextLoc input)) starts
    zOff (prefix, cyc) = genericLength prefix + fromIntegral (fromJust (findIndex (fst >>> last >>> (== 'Z')) cyc))
    eqns = map (zOff &&& (snd >>> genericLength)) cycles

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

mkArray :: IArray UArray a => [a] -> UArray Int a
mkArray as = listArray (0, length as - 1) as
