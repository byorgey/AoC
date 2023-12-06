#!/usr/bin/env stack
-- stack --resolver lts-19.28 script

import Control.Arrow ((>>>))
import Data.List (transpose)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = [Race]
type Race = [Int]

readInput :: String -> Input
readInput = lines >>> map (words >>> drop 1 >>> map read) >>> transpose

-- Naive counting solution, works great
-- waysToWin :: Race -> Int
-- waysToWin [t, d] = count winning [0 .. t]
--   where
--     winning s = s * (t - s) > d

isqrt 1 = 1
isqrt n = go (n `div` 2)
 where
  go d
    | d * d <= n && (d + 1) * (d + 1) > n = d
    | otherwise = go ((d + n `div` d) `div` 2)

mod1 :: Double -> Double
mod1 x = x - fromIntegral (floor x)

-- Fancy solution using quadratic formula, just for fun.  Let t =
-- time, d = distance, sd = sqrt(t^2 - 4d).  Then the solution =
-- number of integers on the open interval (l = (t - sd)/2, r = (t +
-- sd)/2).  The length of the interval is sd, but there might be
-- either (sd - 1), floor(sd), or ceiling(sd) integers in the
-- interval.  We can find out which by (1) checking whether l and r
-- are integers, (2) checking whether {sd} > ceil(l) - l.

waysToWin :: Race -> Int
waysToWin [t, d]
  | isd ^ 2 == disc && t `mod` 2 == isd `mod` 2 = isd - 1
  | mod1 sd > 1 - mod1 l = ceiling sd
  | otherwise = floor sd
 where
  disc = t ^ 2 - 4 * d
  isd = isqrt disc
  sd, l :: Double
  sd = sqrt (fromIntegral disc)
  l = (fromIntegral t - sd) / 2

fixKerning :: [Race] -> [Race]
fixKerning = transpose >>> map (map show >>> concat >>> read) >>> (: [])

type Output = Int

solveA, solveB :: Input -> Output
solveA = map waysToWin >>> product
solveB = fixKerning >>> solveA
