#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((>>>))
import Data.Bifunctor (second)
import Data.Bits
import Data.List (mapAccumL)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

main =
  interact $
    readInput
      >>> applyAll [solveA1, solveA2, solveB1, solveB2]
      >>> map show
      >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = [[Bool]]

readInput :: String -> Input
readInput = lines >>> map (map (/= '.'))

solveA1, solveA2, solveB1, solveB2 :: Input -> Int
solveA1 = solution @Integer >>> snd >>> map popCount >>> sum
solveA2 = solution @(Set Int) >>> snd >>> map S.size >>> sum
solveB1 = solution @[Int] >>> fst >>> sum
solveB2 = solution @(Map Int Int) >>> fst >>> sum

class Beaming s where
  (∩) :: s -> s -> s
  (∪) :: s -> s -> s
  (∸) :: s -> s -> s
  lsh :: s -> s
  rsh :: s -> s
  straight :: s -> s
  fromList :: [Bool] -> s

solution :: Beaming s => [[Bool]] -> (s, [s])
solution (map fromList -> (s : ss)) = mapAccumL split s ss
 where
  split beams splitters = (beams', splits)
   where
    splits = beams ∩ splitters
    beams' = straight (beams ∸ splits) ∪ lsh splits ∪ rsh splits

instance Beaming Integer where
  (∩) = (.&.)
  (∪) = (.|.)
  x ∸ y = x .&. complement y
  lsh = (`shiftL` 1)
  rsh = (`shiftR` 1)
  straight = id
  fromList = foldl' (\n b -> (n `shiftL` 1) .|. (if b then 1 else 0)) 0

instance Beaming (Set Int) where
  (∩) = S.intersection
  (∪) = S.union
  (∸) = S.difference
  lsh = S.map pred
  rsh = S.map succ
  straight = id
  fromList = zip [0 ..] >>> filter snd >>> map fst >>> S.fromList

instance Beaming [Int] where
  (∩) = zipWith (*)
  (∪) = zipWith (+)
  (∸) = zipWith (-)
  lsh ns = tail ns ++ [0]
  rsh ns = 0 : init ns
  straight = id
  fromList = map fromEnum

instance Beaming (Map Int Int) where
  (∩) = M.intersection
  (∪) = M.unionWith (+)
  (∸) = M.difference
  lsh = M.mapKeys pred
  rsh = M.mapKeys succ
  straight = id
  fromList = zip [0 ..] >>> filter snd >>> map (second (const 1)) >>> M.fromList

data Step = L | R | S deriving (Eq, Ord, Show)

instance Beaming (Map Int [[Step]]) where
  (∩) = M.intersection
  (∪) = M.unionWith (++)
  (∸) = M.difference
  lsh = M.mapKeys pred >>> M.map (map (L :))
  rsh = M.mapKeys succ >>> M.map (map (R :))
  straight = M.map (map (S :))
  fromList = zip [0 ..] >>> filter snd >>> map (second (const [[]])) >>> M.fromList
