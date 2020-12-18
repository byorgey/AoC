{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Array
import           Data.Bits
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map             (Map, (!))
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Tuple
import           Text.Parsec          hiding (State)
import           Text.Parsec.String
import           Text.Printf

import           Debug.Trace

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = ()

readInput :: String -> Input
readInput = lines >>> _

type Output = Int

solveA, solveB :: Input -> Output
solveA = const 0
solveB = const 0

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

dfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> a -> [[a]]
dfs winning fnext start = dfs' S.empty [start] start
  where
    dfs' visited path cur
      | winning cur = [path]
      | otherwise = concatMap (\n -> dfs' (S.insert n visited) (n:path) n) next
        where
          next = fnext cur

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

onHead :: (a -> a) -> [a] -> [a]
onHead _ []     = []
onHead f (a:as) = f a : as

infixr 0 >$>
(>$>) = flip ($)

toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
toTable rng f = array rng (map (id &&& f) (range rng))

memo :: Ix i => (i,i) -> (i -> a) -> (i -> a)
memo rng = (!) . toTable rng

memoFix :: Ix i => (i,i) -> ((i -> a) -> (i -> a)) -> (i -> a)
memoFix rng f = fix (memo rng . f)

readParser p = parse p "" >>> either undefined id
