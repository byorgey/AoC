#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow   ((>>>))
import           Data.List       (foldl')
import           Data.List.Split (splitOn)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = splitOn "-" >>> map read

type Output = Int

solveA, solveB :: Input -> Output
solveA [lo,hi] = length
  [ ()
  | a<-[1..9],b<-[a..9],c<-[b..9],d<-[c..9],e<-[d..9],f<-[e..9]
  , a == b || b == c || c == d || d == e || e == f
  , let n = foldl' (\n d -> 10*n + d) 0 [a,b,c,d,e,f]
  , lo <= n && n <= hi
  ]
solveB [lo,hi] = length
  [ ()
  | a<-[1..9],b<-[a..9],c<-[b..9],d<-[c..9],e<-[d..9],f<-[e..9]
  , (a == b && b /= c) || (b == c && a /= b && c /= d)
    || (c == d && b /= c && d /= e) || (d == e && c /= d && e /= f)
    || (e == f && d /= e)
  , let n = foldl' (\n d -> 10*n + d) 0 [a,b,c,d,e,f]
  , lo <= n && n <= hi
  ]

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
