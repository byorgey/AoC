{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow
import           Data.List
import           Data.List.Split

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

data Input = Input { earliestTime :: Integer, schedule :: [Bus] }
data Bus = ID Integer | OutOfService

readInput :: String -> Input
readInput i = Input (read t) sched
  where
    [t, s] = lines i
    sched = map readBus (splitOn "," s)
    readBus "x" = OutOfService
    readBus n   = ID (read n)

type Output = Integer

solveA, solveB :: Input -> Output
solveA (Input{..}) = (t - earliestTime) * b
  where
    inService = [n | ID n <- schedule]
    future = map (\t -> (t, filter ((==0) . (t `mod`)) inService)) [earliestTime ..]
    Just (t, [b]) = find (snd >>> null >>> not) future

solveB (Input{..}) = z `mod` k
  where
    Just (z,k) = gcrt [ (-j, n) | (j, ID n) <- zip [0 ..] schedule]

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

------------------------------------------------------------

-- https://byorgey.wordpress.com/2020/03/03/competitive-programming-in-haskell-modular-arithmetic-part-2/

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0
  | a < 0     = (-a,-1,0)
  | otherwise = (a,1,0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g,x,y) = egcd b (a `mod` b)

-- gcrt solves a system of modular equations.  Each equation x = a
-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that
-- solutions for x satisfy x = z (mod k), that is, solutions are of
-- the form x = z + kt for integer t.
gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt [e]        = Just e
gcrt (e1:e2:es) = gcrt2 e1 e2 >>= \e -> gcrt (e:es)

-- gcrt2 (a,n) (b,m) solves the pair of modular equations
--
--   x = a (mod n)
--   x = b (mod m)
--
-- It returns a pair (c, k) such that all solutions for x satisfy x =
-- c (mod k), that is, solutions are of the form x = c + kt for
-- integer t.
gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a,n) (b,m)
  | a `mod` g == b `mod` g = Just (((a*v*m + b*u*n) `div` g) `mod` k, k)
  | otherwise              = Nothing
  where
    (g,u,v) = egcd n m
    k = (m*n) `div` g
