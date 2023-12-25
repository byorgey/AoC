#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package linear --package gloss

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow (first, second, (***), (>>>))
import Control.Monad (guard)
import Data.Foldable qualified as F
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Geom qualified
import Graphics.Gloss hiding (Point)
import Linear.Affine
import Linear.Metric
import Linear.V3
import Linear.Vector

main = solveMain

-- Some visualization code.

glossMain2 = do
  input <- readInput <$> readFile "input"
  let pvs = map toPV input
      c = crossover pvs
  display (InWindow "gloss" (600, 600) (0, 0)) white (Pictures (map drawPt pvs ++ [color red (drawPt (fst c))]))

drawPt (p, v) = translate (toPic (10 ^ 15) p) (toPic 500 v) (circleSolid 1)
 where
  toPic s t = (fromIntegral t / (3 * s) + 1) * 300

glossMain = do
  input <- readInput <$> readFile "input"
  animate (InWindow "gloss" (600, 600) (10, 10)) white (drawPts input)

drawPts :: [Hailstone Int] -> Float -> Picture
drawPts (h : hs) t = Pictures $ color red (drawHailstone t h) : map (drawHailstone t) hs

drawHailstone :: Float -> Hailstone Int -> Picture
drawHailstone t (Hailstone (P (V3 px py pz)) (V3 vx vy vz)) =
  translate (toPic px vx) (toPic py vy) (circleSolid 1)
 where
  s = mod1 (t / 15)
  t' = s * 15 * 50000000000
  toPic p v = ((fromIntegral p + t' * fromIntegral v) / 10 ^ 15 + 1) * 300

mod1 :: Float -> Float
mod1 x = x - fromIntegral (floor x)

solveMain =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

listToV3 :: [a] -> V3 a
listToV3 [x, y, z] = V3 x y z

type P3 = Point V3
data Hailstone s = Hailstone {pos :: P3 s, vel :: V3 s} deriving (Functor)
type Input = [Hailstone Int]

readInput :: String -> Input
readInput = lines >>> map readHailstone

readHailstone :: String -> Hailstone Int
readHailstone =
  filter (`notElem` ",@") >>> words >>> map read >>> chunksOf 3 >>> \[p, v] -> Hailstone (P (listToV3 p)) (listToV3 v)

type Output = Int

solveA, solveB :: Input -> Output
solveA = pairs >>> count (uncurry (collideInside2D 200000000000000 400000000000000))
solveB input = crossover pvs >$> solveCrossover pvs >>> fromJust >>> fst >>> fromIntegral
 where
  pvs = map toPV input

toPV (Hailstone (P (V3 px py pz)) (V3 vx vy vz)) = (px + py + pz, vx + vy + vz)

------------------------------------------------------------

collideInside2D :: Int -> Int -> Hailstone Int -> Hailstone Int -> Bool
collideInside2D lo hi h1 h2 = case i of
  Nothing -> False
  Just p@(Geom.V2 px py) ->
    and
      [ lo' <= px
      , px <= hi'
      , lo' <= py
      , py <= hi'
      , Geom.dot v1 (p Geom.^-^ p1) > 0
      , Geom.dot v2 (p Geom.^-^ p2) > 0
      ]
 where
  [lo', hi'] = map fromIntegral [lo, hi]
  (p1, l1@(Geom.L2 v1 _)) = hailstoneToP2L2 h1
  (p2, l2@(Geom.L2 v2 _)) = hailstoneToP2L2 h2
  i = Geom.linesIntersect l1 l2

hailstoneToP2L2 :: Hailstone Int -> (Geom.P2 Double, Geom.L2 Double)
hailstoneToP2L2 (Hailstone (P (V3 px py _)) (V3 vx vy _)) =
  (p2, Geom.lineFromPoints p2 (p2 Geom.^+^ v2))
 where
  p2 = Geom.V2 (fromIntegral px) (fromIntegral py)
  v2 = Geom.V2 (fromIntegral vx) (fromIntegral vy)

------------------------------------------------------------

-- The square of the closest approach of two hailstones
-- Didn't end up using this.
minDistSq :: Hailstone Double -> Hailstone Double -> Double
minDistSq (Hailstone p1 v1) (Hailstone p2 v2) = d
 where
  t = dot (p1 .-. p2) (v1 ^-^ v2)
  d = quadrance ((p1 .-. p2) + t *^ (v1 ^-^ v2))

sosd :: Hailstone Double -> [Hailstone Double] -> Double
sosd h hs = sum $ map (minDistSq h) hs

-- Assuming we are given a list of points which all lie in the 2nd or
-- 4th quadrant relative to some "crossover" point, find a range of
-- viable crossover points.
crossover :: [(Int, Int)] -> ((Int, Int), (Int, Int))
crossover = sortBy (comparing (uncurry (-))) >>> splits >>> map findCrossover >>> F.asum >>> fromJust
 where
  findCrossover ((ax, ay) : as, (bx, by) : bs) = ((ax + 1, ay - 1), (bx - 1, by + 1)) <$ guard (isCrossover (ax + 1, ay - 1))
   where
    isCrossover (x, y) = all ((< x) . fst) as && all ((> y) . snd) as && all ((> x) . fst) bs && all ((< y) . snd) bs

-- The crossover is somewhere in the range
-- ((930599464652786,-173),(930878324098179,-170)).  For each
-- velocity, we can solve a system of modular equations to find a
-- position that yields all integer values for t.

solveCrossover :: [(Int, Int)] -> ((Int, Int), (Int, Int)) -> Maybe (Integer, Integer)
solveCrossover pvs ((plo, vlo), (phi, vhi)) = F.asum (map solveP [vlo .. vhi])
 where
  solveP v = gcrt (map ((fromIntegral *** fromIntegral) . second (abs . (v -))) pvs)

------------------------------------------------------------
-- Utilities

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

splits :: [a] -> [([a], [a])]
splits (a : as) = ([a], as) >$> iterate step >>> takeWhile (snd >>> null >>> not)
 where
  step (as, b : bs) = (b : as, bs)

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

-- gcrt solves a system of modular equations.  Each equation x = a
-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that
-- solutions for x satisfy x = z (mod k), that is, solutions are of
-- the form x = kt + c for integer t.
gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt [e] = Just e
gcrt (e1 : e2 : es) = gcrt2 e1 e2 >>= \e -> gcrt (e : es)

-- gcrt2 (a,n) (b,m) solves the pair of modular equations
--
--   x = a (mod n)
--   x = b (mod m)
--
-- It returns a pair (c, k) such that all solutions for x satisfy x =
-- c (mod k), that is, solutions are of the form x = kt + c for
-- integer t.
gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a, n) (b, m)
  | a `mod` g == b `mod` g = Just (((a * v * m + b * u * n) `div` g) `mod` k, k)
  | otherwise = Nothing
 where
  (g, u, v) = egcd n m
  k = (m * n) `div` g

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0
  | a < 0 = (-a, -1, 0)
  | otherwise = (a, 1, 0)
egcd a b = (g, y, x - (a `div` b) * y)
 where
  (g, x, y) = egcd b (a `mod` b)
