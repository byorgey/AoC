#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array --package MIP --package OptDir --package bytestring-encoding --package text

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow ((>>>))
import Data.Bits
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Numeric.Optimization.MIP ((.==.))
import Numeric.Optimization.MIP qualified as MIP
import Numeric.Optimization.MIP.Solver

main = do
  input <- readInput <$> getContents
  print $ solveA input

  resB <- solveB input
  print resB

type Lights = Integer
type Input = [Machine]
data Machine = Machine {lights :: Lights, buttons :: [[Int]], joltage :: [Int]}
  deriving (Show)

readInput :: String -> Input
readInput = lines >>> map readMachine

readMachine :: String -> Machine
readMachine s = Machine (readLights w) (map readButton (init ws)) (readJoltage (last ws))
 where
  (w : ws) = words s

  snip = drop 1 . reverse . drop 1 . reverse

  readLights = snip >>> foldr (\b n -> (if b == '#' then 1 else 0) .|. (n `shiftL` 1)) 0
  readButton = snip >>> splitOn "," >>> map read
  readJoltage = snip >>> splitOn "," >>> map read

type Output = Int

------------------------------------------------------------
-- Part A solution

solveA :: Input -> Output
solveA = map solveMachineA >>> sum

-- Part A: BFS over bitvectors.  Search space is at most 2^10.
solveMachineA (Machine ls bs _) = pred . length $ bfs (== ls) next (S.singleton 0)
 where
  buttonLights = bs >$> map (map (1 `shiftL`) >>> foldl' (.|.) 0)
  next m = S.fromList $ map (m `xor`) buttonLights

------------------------------------------------------------
-- Part B failed attempts

-- Initial attempt via BFS, took forever and used up all my memory.
-- This makes sense, actually: if we have 10 counters each trying to
-- reach values in the 100's, the search space would be on the order
-- of 100^10, much too large.

-- solveMachineB (Machine _ bs js) = pred . length $ bfs (== js) next (S.singleton (map (const 0) js))
--  where
--   buttonCounters = map (elems . accumArray @UArray (\_ y -> y) 0 (0, length js - 1) . map (,1)) bs
--   next m = S.fromList . filter ok $ map (zipWith (+) m) buttonCounters
--   ok cs = and (zipWith (<=) cs js)

-- We're looking for positive k_i such that sum (k_i * c_i) = j where
-- c_i are the (0,1)-vectors each button, subject to the constraint
-- that sum k_i is as small as possible.  Sounds like ILP...
--
-- More specifically, let:
--   - B = the (0,1) matrix whose columns correspond to buttons
--   - k = a column vector representing the number of times we push each button
--   - j = a column vector representing the desired joltage counter values
--
-- Then we want to solve  B k = j  for k while minimizing the sum of k.
--
-- We can solve B k = j using Gaussian elimination; but what if there
-- are multiple solutions?  We're guaranteed there will be at least
-- one solution...
--
-- Ah, how about BSTA?  For each particular sum, we can just add an
-- extra equation specifying that the sum of all k = that sum.  Then
-- check if there is a solution or not.  Hmm, but binary search
-- doesn't work though, since this is not monotonic...

-- DFS, greedily choosing biggest button possible at each step ---
-- doesn't give correct result.

-- solveMachineB (Machine _ bs js) = length . head $ dfs (== js) next (map (const 0) js)
--  where
--   bs' = sortBy (comparing (Down . length)) bs -- prefer bigger buttons
--   buttonCounters = map (elems . accumArray @UArray (\_ y -> y) 0 (0, length js - 1) . map (,1)) bs'
--   next m = S.fromList . filter ok $ map (zipWith (+) m) buttonCounters
--   ok cs = and (zipWith (<=) cs js)

------------------------------------------------------------
-- Part B solution

-- Finally found a good Haskell library for mixed integer programming,
-- once I figured out the right keywords to search for.

solveB :: Input -> IO Output
solveB ms = sum <$> mapM solveMachineB ms

solveMachineB :: Machine -> IO Output
solveMachineB (Machine _ bs js) = do
  let -- xi = how many times we should push button i
      xsN = map (MIP.Var . ("x" <>) . T.pack . show) [0 .. length bs - 1]
      xs = map MIP.varExpr xsN

      counterConstraint j c = sum (map fst . filter ((j `elem`) . snd) $ zip xs bs) .==. fromIntegral c

      prob =
        MIP.def
          { MIP.objectiveFunction =
              MIP.def
                { MIP.objDir = MIP.OptMin
                , MIP.objExpr = sum xs
                }
          , MIP.constraints = zipWith counterConstraint [0 ..] js
          , MIP.varDomains = M.fromList (map (,(MIP.IntegerVariable, (0, MIP.PosInf))) xsN)
          }

  sol <- solve cbc MIP.def {solveTimeLimit = Just 1.0} prob
  pure (round (fromMaybe 0 (MIP.solObjectiveValue sol)))

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> Set a) -> Set a -> [Set a]
bfs isGoal next = bfs' S.empty
 where
  bfs' seen layer
    | S.null layer = []
    | any isGoal layer = [layer]
    | otherwise = layer : bfs' seen' layer'
   where
    layer' = foldMap next layer `S.difference` seen'
    seen' = S.union seen layer

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
