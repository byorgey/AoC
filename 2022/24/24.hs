#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed
import           Data.List          (find)
import           Data.Maybe         (fromJust)
import           Data.Set           (Set)
import qualified Data.Set           as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = UArray Coord Char

readInput :: String -> Input
readInput = lines >>> trim >>> map trim >>> mkArray

trim = init >>> drop 1

------------------------------------------------------------

type Time = Int

mkMap :: UArray Coord Char -> UArray (Time, Int, Int) Bool
mkMap blizzards = accumArray (\_ t -> t) False ((0,0,0),(time - 1, rows - 1, cols - 1)) blocked
  where
    time = lcm rows cols
    (_, (succ -> rows, succ -> cols)) = bounds blizzards
    blocked = concatMap genBlocked (assocs blizzards)
    genBlocked (_, '.') = []
    genBlocked (p, b) = zipWith (\t (r,c) -> ((t,r,c),True)) [0 .. time-1] (iterate (interpDir b >>> wrap) p)

    interpDir '^' = above
    interpDir '<' = left
    interpDir 'v' = below
    interpDir '>' = right
    wrap (r,c) = (r `mod` rows, c `mod` cols)

------------------------------------------------------------

data Pos = Start Int | Middle (Int,Int,Int) | End Int
  deriving (Eq, Ord, Show)

getTime (Start t)        = t
getTime (Middle (t,_,_)) = t
getTime (End t)          = t

isEnd (End{}) = True
isEnd _       = False

isStart (Start{}) = True
isStart _         = False

solveA :: Input -> Int
solveA = mkMap >>> search (Start 0) isEnd >>> getTime

solveB :: Input -> Int
solveB bs = getTime p3
  where
    blocked = mkMap bs
    p1 = search (Start 0) isEnd blocked
    p2 = search p1 isStart blocked
    p3 = search p2 isEnd blocked

search :: Pos -> (Pos -> Bool) -> UArray (Time, Int, Int) Bool -> Pos
search from goal blocked = bfs goal next (S.singleton from) >$> last >>> S.toList >>> find' goal
  where
    (_, (succ -> time, succ -> rows, succ -> cols)) = bounds blocked
    ok (Middle (t,r,c)) = not (blocked!(t `mod` time, r, c))
    ok _                = True
    next = candidates >>> filter ok >>> S.fromList
    candidates (Start t) = [Start (t+1), Middle (t+1,0,0)]
    candidates (End t) = [End (t+1), Middle (t+1,rows-1,cols-1)]
    candidates (Middle (t,r,c)) =
        [ Middle (t+1,r',c')
        | (r',c') <- (r,c) : neighbors (r,c)
        , 0 <= r' && r' < rows && 0 <= c' && c' < cols
        ]
        ++
        [ Start (t+1) | (r,c) == (0,0) ]
        ++
        [ End (t+1) | (r,c) == (rows-1, cols-1) ]

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | S.null layer     = []
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = foldMap next layer `S.difference` seen'
          seen' = S.union seen layer

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int,Int)

above, below, left, right :: Coord -> Coord
above (r,c) = (r-1,c)
below (r,c) = (r+1,c)
left (r,c) = (r,c-1)
right (r,c) = (r,c+1)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0,0), (length rows - 1, length (head rows) - 1)) (concat rows)

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, left, right]
