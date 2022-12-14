#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Arrow      ((&&&), (***), (>>>))
import           Data.Array.Unboxed
import           Data.Function      (on)
import           Data.List          (find, groupBy)
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           Data.Ord
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Prelude            hiding (drop)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Coord]]

readInput :: String -> Input
readInput = lines >>> map (words >>> filter (/="->") >>> map (splitOn "," >>> map read >>> toPair))
  where
    toPair [x,y] = (y,x)

------------------------------------------------------------

-- Turning input into array

drawMap :: Input -> UArray Coord Bool
drawMap ls = ls >$> concatMap drawPath >>> accumArray (\_ t -> t) False bds
  where
    bds = ls >$> concat >>> ((0,500):) >>> unzip >>> (minmax *** minmax) >>> mkBounds
    minmax = minimum &&& maximum
    mkBounds ((rmin,rmax),(cmin,cmax)) = ((rmin,cmin),(rmax,cmax))

drawPath :: [Coord] -> [(Coord, Bool)]
drawPath = tracePath >>> map (,True)

tracePath :: [Coord] -> [Coord]
tracePath cs = concatMap (uncurry traceEdge) (zip cs (tail cs))

traceEdge :: Coord -> Coord -> [Coord]
traceEdge p1@(r1,c1) p2@(r2,c2) = takeUntil (==p2) (iterate step p1)
  where
    step = case (compare r1 r2, compare c1 c2) of
      (LT,_) -> below
      (GT,_) -> above
      (_,LT) -> right
      (_,GT) -> left


-- For debugging
showMap :: UArray Coord Bool -> String
showMap m = unlines $ [[if m ! p then '#' else '.' | p <- l] | l <- coords]
  where
    coords = groupBy ((==) `on` fst) $ range (bounds m)

------------------------------------------------------------
-- Part 1

fall :: UArray Coord Bool -> Set Coord -> Coord -> Maybe Coord
fall cave sand c = find unblocked (applyAll [below, left . below, right . below] c)
  where
    unblocked p = not (inRange (bounds cave) p) || (not (cave ! p) && p `S.notMember` sand)

drop :: UArray Coord Bool -> Set Coord -> Maybe (Set Coord)
drop cave sand = go (0,500)
  where
    go p = case fall cave sand p of
      Nothing -> Just (S.insert p sand)
      Just p' -> if inRange (bounds cave) p' then go p' else Nothing

fill :: UArray Coord Bool -> Set Coord
fill cave = go S.empty
  where
    go sand = maybe sand go (drop cave sand)

------------------------------------------------------------
-- Part 2

(!?) :: (IArray UArray e, Ix i) => UArray i e -> i -> Maybe e
a !? i
  | inRange (bounds a) i = Just (a ! i)
  | otherwise = Nothing

nextLine :: UArray Coord Bool -> (Int,UArray Int Bool) -> (Int,UArray Int Bool)
nextLine cave (r, sand) = (r+1, array (bounds sand) [(i,isSand i) | i <- range (bounds sand)])
  where
    isSand i =
      maybe True not (cave !? (r+1,i)) && or (mapMaybe (sand !?) [i-1,i,i+1])

genPyramid :: UArray Coord Bool -> [(Int,UArray Int Bool)]
genPyramid cave = take (h+2) $ iterate (nextLine cave) (0, accumArray (\_ x -> x) False (500 - (h+2), 500 + (h+2)) [(500, True)])
  where
    (_, (h,_)) = bounds cave

countSand :: [(Int, UArray Int Bool)] -> Int
countSand = map snd >>> map (elems >>> count id) >>> sum

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA = drawMap >>> fill >>> S.size
solveB = drawMap >>> genPyramid >>> countSand

------------------------------------------------------------
-- Utility

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int,Int)

above, below, left, right :: Coord -> Coord
above (r,c) = (r-1,c)
below (r,c) = (r+1,c)
left (r,c) = (r,c-1)
right (r,c) = (r,c+1)

