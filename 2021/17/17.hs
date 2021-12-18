#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Control.Arrow ((>>>))
import           Data.Char     (isDigit)
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Range = (Int,Int)
type Target = (Range,Range)

readInput :: String -> Target
readInput = map (\c -> if isDigit c || c == '-' then c else ' ') >>> words >>> map read
  >>> (\[xmin,xmax,ymin,ymax] -> ((xmin,xmax), (ymin,ymax)))

type Output = Int

solveA, solveB :: Target -> Output
solveA = findVs >>> filter ((!!1) >>> (>= 0)) >>> map apogee >>> maximum
solveB = findVs >>> length

findVs t@((xmin,xmax),(ymin,ymax)) =
  filter (hits t) [[vx,vy] | vx <- [0 .. xmax], vy <- [ymin .. -ymin]]

------------------------------------------------------------

type V2 = [Int]
data St = St { position :: V2, velocity :: V2 }

initSt :: V2 -> St
initSt = St [0,0]

trajectory :: St -> [St]
trajectory = iterate step
  where
    step (St p v@[vx,vy]) = St (zipWith (+) p v) [max 0 (vx - 1), vy - 1]

inTarget :: Target -> V2 -> Bool
inTarget (xr,yr) [x,y] = (x `inRange` xr) && (y `inRange` yr)

inRange :: Int -> Range -> Bool
inRange x (lo,hi) = lo <= x && x <= hi

hits :: Target -> V2 -> Bool
hits t@((_,xmax),(ymin,_)) = initSt >>> trajectory >>>
  takeWhile (\(St [x,y] _) -> x <= xmax && y >= ymin) >>> any (position >>> inTarget t)

apogee :: V2 -> Int
apogee = initSt >>> trajectory >>> find' (velocity >>> (!!1) >>> (==0)) >>> position >>> (!!1)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)
