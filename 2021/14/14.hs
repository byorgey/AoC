#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Arrow ((>>>))
import           Data.List (sort)
import           Data.Map            (Map, (!))
import qualified Data.Map.Strict     as M

------------------------------------------------------------

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Digraph = (Char,Char)
type Rules = Map Digraph Char
type Count a = Map a Int
data Input = Input { template :: String, rules :: Rules }
  deriving Show

readInput :: String -> Input
readInput = lines >>> (\(t:_:rs) -> Input t (readRules rs))

readRules = map (words >>> (\[[x,y],_,[z]] -> ((x,y),z))) >>> M.fromList

type Output = Int

solveA, solveB :: Input -> Output
solveA = solve 10
solveB = solve 40

solveNaive :: Int -> Input -> Output
solveNaive n Input{..} =
  template >$> iterate (insertionStep rules) >>> (!!n) >>> cardinality >>> countGap

solve :: Int -> Input -> Output
solve n Input{..} =
  template >$> digraphCount >>> iterate (insertionStepByDigraph rules)
  >>> (!!n) >>> digraphToCharCount (last template) >>> countGap

------------------------------------------------------------

countGap :: Count a -> Int
countGap m = last cs - head cs
  where
    cs = sort (M.elems m)

insertionStep :: Rules -> String -> String
insertionStep m s@(c:_) = c : concat (zipWith (\x y -> [m!(x,y), y]) s (tail s))

digraphCount :: String -> Count Digraph
digraphCount = (zip <*> tail) >>> cardinality

insertionStepByDigraph :: Rules -> Count Digraph -> Count Digraph
insertionStepByDigraph m = M.assocs >>> concatMap generate >>> M.fromListWith (+)
  where
    generate (d@(x,y),n) = [((x,m!d),n), ((m!d,y),n)]

digraphToCharCount :: Char -> Count Digraph -> Count Char
digraphToCharCount final = M.mapKeysWith (+) fst >>> M.adjust (+1) final

------------------------------------------------------------

cardinality :: Ord a => [a] -> Map a Int
cardinality = map (,1) >>> M.fromListWith (+)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
