#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Control.Arrow      ((***), (>>>))
import           Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as U
import           Data.Function      (on)
import           Data.List          (findIndex, groupBy)
import           Data.Maybe         (fromJust)

main = interact $ readInput >>> solve >>> show >>> (++"\n")

infixr 0 >$>
(>$>) = flip ($)

type Grid = UArray (Int,Int) Char

readInput :: String -> Grid
readInput = lines >>> mkArray
  where
    mkArray ls = U.listArray ((0,0),(r-1,c-1)) (concat ls)
      where
        r = length ls
        c = length (head ls)

type Output = Int

solve :: Grid -> Output
solve = iterate (step E >>> step S) >>>
  (zip <*> tail) >>> findIndex (uncurry (==)) >>> fromJust >>> succ

------------------------------------------------------------

showGrid :: Grid -> String
showGrid = U.assocs >>> groupBy ((==) `on` (fst.fst)) >>> map (map snd) >>> unlines

printGrid :: Grid -> IO ()
printGrid = putStr . showGrid

data Dir = S | E deriving (Eq, Ord, Show)

dirChar :: Dir -> Char
dirChar S = 'v'
dirChar E = '>'

prev, next :: (Int,Int) -> Dir -> (Int,Int) -> (Int,Int)
prev (r,c) S (i,j) = ((i-1) `mod` r, j)
prev (r,c) E (i,j) = (i, (j-1) `mod` c)
next (r,c) S (i,j) = ((i+1) `mod` r, j)
next (r,c) E (i,j) = (i, (j+1) `mod` c)

step :: Dir -> Grid -> Grid
step dir g = U.array ((0,0),(r-1,c-1))
  [ ((i,j), x)
  | i <- [0..r-1], j <- [0..c-1]
  , let x
          | g!(i,j) == '.' && (g!(prev (r,c) dir (i,j)) == dirChar dir) = dirChar dir
          | g!(i,j) == dirChar dir && g!(next (r,c) dir (i,j)) == '.'   = '.'
          | otherwise = g!(i,j)
  ]
  where
    (r,c) = g >$> U.bounds >>> snd >>> (succ *** succ)
