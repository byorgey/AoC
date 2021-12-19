#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

import           Control.Arrow       (second, (***), (>>>))
import           Control.Monad.State (State, execState, forM_, get, modify,
                                      when)
import qualified Data.Foldable       as F
import           Data.List           (find, foldl1', nub)
import           Data.List.Split     (splitOn)
import           Data.Map            (Map, (!))
import qualified Data.Map.Strict     as M
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Tuple          (swap)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type P3 = [Int]
type Scanner = [P3]
type Input = [Scanner]

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> map (drop 1 >>> map (splitOn "," >>> map read))

type Output = Int

solveA, solveB :: Input -> Output
solveA = alignAll >>> coalesce >>> length
solveB = alignAll >>> M.elems >>> map snd >>> pairs >>> map (uncurry manhattan) >>> maximum

------------------------------------------------------------

(^+^), (^-^) :: P3 -> P3 -> P3
(^+^) = zipWith (+)
(^-^) = zipWith (-)

manhattan :: P3 -> P3 -> Int
manhattan p q = zipWith (-) p q >$> map abs >>> sum

distSq :: P3 -> P3 -> Int
distSq p q = (p ^-^ q) >$> map (^2) >>> sum

dists :: Scanner -> [Int]
dists = pairs >>> map (uncurry distSq)

overlapBy :: Int -> Scanner -> Scanner -> Bool
overlapBy n s1 s2 = inCommon (dists s1) (dists s2) >= (n*(n-1) `div` 2)

inCommon :: Ord a => [a] -> [a] -> Int
inCommon xs ys = MS.size (MS.fromList xs `MS.intersection` MS.fromList ys)

overlaps :: [Scanner] -> [(Int,Int)]
overlaps = zip [0..] >>> pairs >>>
  filter (\((_,si),(_,sj)) -> overlapBy 12 si sj) >>> map (fst *** fst)

scannerGraph :: [Scanner] -> Map Int (Set Int)
scannerGraph = overlaps >>> mapFromEdges

cubeSymmetries :: [P3 -> P3]
cubeSymmetries =
  [ twist >>> face
  | twist <- take 4 (iterate (rotx.) id)
  , face <- take 4 (iterate (roty.) id) ++ [rotz, rotz.rotz.rotz]
  ]
  where
    rotx [x,y,z] = [x,-z,y]
    roty [x,y,z] = [-z,y,x]
    rotz [x,y,z] = [-y,x,z]

-- Align the data from two scanners.  Find the best alignment and
-- return a rotated and translated version of the second one, along
-- with the translation used.
align :: Int -> Scanner -> Scanner -> Maybe (Scanner, P3)
align n s1 s2 = F.asum $ map (shiftAlign n s1) rots
  where
    rots = [map sym s2 | sym <- cubeSymmetries]

-- Try to align data from two scanners by only translating the second
-- one.  Return the translated data from the second scanner (and the
-- translation amount) if we find one that makes them match up.  Just
-- try every possible translation that moves a point from the second
-- scanner onto a point from the first.
shiftAlign :: Int -> Scanner -> Scanner -> Maybe (Scanner, P3)
shiftAlign n s1 s2 = find (\(s2', _) -> inCommon s1 s2' >= n) shifts
  where
    offsets = [p1 ^-^ p2 | p1 <- s1, p2 <- s2]
    shifts  = map (\v -> (map (v ^+^) s2, v)) offsets

-- Stitch two sets of scanner data into one, dropping any duplicates.
-- The intention is that they have already been aligned, although the
-- function doesn't actually care.
stitch :: Scanner -> Scanner -> Scanner
stitch s1 s2 = nub (s1 ++ s2)

coalesce :: Map Int (Scanner, P3) -> Scanner
coalesce = M.elems >>> map fst >>> foldl1' stitch

alignAll :: [Scanner] -> Map Int (Scanner, P3)
alignAll ss = execState (dfs 0) (M.singleton 0 (ss!!0, [0,0,0]))
  where
    g = scannerGraph ss

    dfs :: Int -> State (Map Int (Scanner, P3)) ()
    dfs u = forM_ (g ! u) $ \v -> do
      m <- get
      when (M.notMember v m) $ do
        let Just sv' = align 12 (fst (m!u)) (ss!!v)
        modify $ M.insert v sv'
        dfs v

------------------------------------------------------------
-- Utilities

mapFromEdges :: Ord a => [(a,a)] -> Map a (Set a)
mapFromEdges
  = concatMap (applyAll [id,swap]) >>> map (second S.singleton) >>> M.fromListWith S.union

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
