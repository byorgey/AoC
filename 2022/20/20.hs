#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package mtl --package array

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Array.Unboxed
import           Data.Semigroup      (Endo (..))
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq

------------------------------------------------------------
-- Input

type Input = [Int]

readInput :: String -> Input
readInput = lines >>> map read

------------------------------------------------------------
-- Utilities

-- Like mod, but (`mod1` n) returns an answer in the range [1..n].
mod1 :: Int -> Int -> Int
x `mod1` n = case x `mod` n of
  0  -> n
  x' -> x'

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

------------------------------------------------------------
-- Permutations

-- Shift f t means index f shifts to t and everything in between
-- shifts over to make room.
data Perm = Shift Int Int | P (UArray Int Int)
  deriving (Eq, Show)

applyShift :: Int -> Int -> Int -> Int
applyShift f t i
  | i == f = t
  | f < t && f < i && i <= t = i - 1
  | f > t && f > i && i >= t = i + 1
  | otherwise = i

applyPerm :: Perm -> Int -> Int
applyPerm (Shift f t) i = applyShift f t i
applyPerm (P a) i       = a!i

applyPerms :: [Perm] -> Int -> Int
applyPerms = appEndo . foldMap (Endo . applyPerm)

-- Speed up application of a sequence of permutations by consolidating
-- them into a single array lookup.
consolidate :: Int -> [Perm] -> [Perm]
consolidate _ [] = []
consolidate n ps = [P $ array (0,n-1) [(i, applyPerms ps i) | i <- [0 .. n-1]]]

------------------------------------------------------------
-- Simulation

-- file :: Seq Int is the current state of the file. Data.Seq gives us
-- O(lg n)-time delete and insert.
data St = St { perm :: [Perm], file :: Seq Int }

initSt :: [Int] -> St
initSt xs = St [] (Seq.fromList xs)

mix :: State St ()
mix = do
  St s f <- get
  let n = Seq.length f
  put $ St (consolidate n s) f
  mapM_ mixIx [0 .. n-1]

mixIx :: Int -> State St ()
mixIx k = do
  St s f <- get
  let n = Seq.length f
      k' = applyPerms s k
      v = f `Seq.index` k'
      k'' = (k' + v) `mod1` (n - 1)

      f' = Seq.deleteAt k' f
      f'' = Seq.insertAt k'' v f'

  put ({- traceShow f $ traceShow s $ -} St (Shift k' k'' : s) f'')

getGroveCoords :: St -> Int
getGroveCoords (St _ f) = sum $ map ((f `Seq.index`) . (`mod` n) . (i+)) [1000,2000,3000]
  where
    n = Seq.length f
    Just i = Seq.elemIndexL 0 f

------------------------------------------------------------
-- Solution

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Output = Int

decryptKey = 811589153

solveA, solveB :: Input -> Output
solveA = initSt >>> execState mix >>> getGroveCoords
solveB = map (*decryptKey) >>> initSt >>> execState (replicateM 10 mix) >>> getGroveCoords

