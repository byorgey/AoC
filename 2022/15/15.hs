#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package data-interval

import           Control.Arrow    (second, (>>>))
import           Data.Char        (isDigit)
import           Data.Interval
import           Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import           Data.List        (find, foldl')
import           Data.List.Split  (chunksOf)
import           Data.Maybe       (fromJust, isJust, listToMaybe)
import           Data.Tuple       (swap)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [(Coord, Coord)]

readInput :: String -> Input
readInput = lines >>> map (map keepDigits >>> words >>> map read >>> chunksOf 2 >>> map toPair >>> toPair)
  where
    keepDigits c
      | isDigit c || c == '-' = c
      | otherwise = ' '

------------------------------------------------------------

manhattan :: Coord -> Coord -> Int
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

intervalAtY :: Int -> (Coord, Coord) -> Interval Int
intervalAtY y (sensor@(sx,sy), beacon@(bx,by)) = Finite (sx - offset) <=..< Finite (sx + offset + 1)
  where
    d = manhattan sensor beacon
    h = abs (sy - y)
    offset = d - h

intervalSetAtY :: Int -> [(Coord, Coord)] -> IntervalSet Int
intervalSetAtY y = map (intervalAtY y >>> IS.singleton) >>> IS.unions

getGap :: Int -> IntervalSet Int -> Maybe Int
getGap search = IS.toList >>> map (upperBound >>> getF) >>> filter (\x -> 0 <= x && x <= search) >>> listToMaybe
  where
    getF (Finite n) = n

hasGap search = getGap search >>> isJust

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA i = i >$> intervalSetAtY y >>> flip (foldl' IS.difference) bs >>> IS.toList >>> map width >>> sum
  where
    bs = map (snd >>> fst >>> (\x -> Finite x <=..< Finite (x+1)) >>> IS.singleton) $ filter (snd >>> snd >>> (==y)) i
    y = 2000000

solveB i = x*4000000 + y
  where
    search = 4000000
    (x,y) = swap . second (getGap search >>> fromJust) . findI' (hasGap search) $ map (flip intervalSetAtY i) [0 .. search]

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toPair :: [a] -> (a,a)
toPair [x,y] = (x,y)

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

findI' :: (a -> Bool) -> [a] -> (Int,a)
findI' p = zip [0..] >>> find' (snd >>> p)

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int,Int)
