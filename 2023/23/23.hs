#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package array --package mtl

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Applicative
import Control.Arrow ((&&&), (>>>))
import Control.Monad (when)
import Control.Monad.State
import Data.Array.Unboxed
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = UArray Coord Char

readInput :: String -> Input
readInput = lines >>> mkArray

type Output = Int

solveA, solveB :: Input -> Output
solveA = mkGraph >>> longestPaths >>> (M.! (0, 1))
solveB g = g >$> mkUGraph >>> longestUPath (0, 1) dest
 where
  (_, (rmax, cmax)) = bounds g
  dest = (rmax, cmax - 1)

------------------------------------------------------------

data Step = Step {loc :: Coord, dir :: Vec}

initStep :: Step
initStep = Step (0, 1) (1, 0)

-- Create a directed graph out of the input grid.  We assume it is a
-- DAG.  Each location only maps to other locations we could possibly
-- visit next, if we are obeying all one-way slopes.  So typically one
-- location will have only one neighbor, with only a few locations
-- having multiple choices.
mkGraph :: Input -> Map Coord [Coord]
mkGraph grid = execState (dfs initStep) M.empty
 where
  dfs :: Step -> State (Map Coord [Coord]) ()
  dfs s@(Step c d) = do
    g <- get
    when (c `M.notMember` g) $ do
      modify $ M.insert c (map loc ns)
      mapM_ dfs ns
   where
    ns = filter ok . map mkStep $ neighborsIn grid c
    mkStep n = Step n (n ^-^ c)
    ok (Step n d') =
      and
        [ c ^-^ n /= dir s
        , grid ! n /= '#'
        , grid ! n == '.' || matches (grid ! n) d'
        ]

matches :: Char -> Vec -> Bool
matches c v = dirChar M.! v == c

dirChar :: Map Vec Char
dirChar = M.fromList [((-1, 0), '^'), ((0, 1), '>'), ((1, 0), 'v'), ((0, -1), '<')]

longestPaths :: Map Coord [Coord] -> Map Coord Int
longestPaths m = longest
 where
  longest = M.map (map ((longest M.!) >>> succ) >>> maximum0) m

maximum0 [] = 0
maximum0 xs = maximum xs

drawGraph :: Input -> Map Coord [Coord] -> String
drawGraph grid g = grid >$> assocs >>> groupBy ((==) `on` (fst >>> fst)) >>> map (map drawCell) >>> unlines
 where
  drawCell (_, '#') = '#'
  drawCell (c, '.') =
    case M.lookup c g of
      Just [n] -> dirChar M.! (n ^-^ c)
      _ -> '.'
  drawCell (_, x) = x

------------------------------------------------------------

-- Make an *undirected* graph, where the only vertices are
-- coordinates with degree > 2, + start and end locations.
mkUGraph :: Input -> Map Coord [(Coord, Int)]
mkUGraph grid = M.fromList $ map (id &&& connections grid) vs
 where
  vs = filter (isVertex grid) (range (bounds grid))

isVertex :: Input -> Coord -> Bool
isVertex grid c =
  (grid ! c) /= '#' && (c == (0, 1) || c == (rmax, cmax - 1) || degree c > 2)
 where
  (_, (rmax, cmax)) = bounds grid
  degree = openNeighbors grid >>> length

openNeighbors grid = neighborsIn grid >>> filter ((grid !) >>> (/= '#'))

connections :: Input -> Coord -> [(Coord, Int)]
connections grid v = v >$> openNeighbors grid >>> map (followPath grid v 1)

followPath :: Input -> Coord -> Int -> Coord -> (Coord, Int)
followPath grid !parent !len !cur
  | isVertex grid cur = (cur, len)
  | otherwise = followPath grid cur (len + 1) (head (filter (/= parent) (openNeighbors grid cur)))

-- Find longest path by brute force, not a problem since there are
-- only 9 vertices.
longestUPath :: (Ord a, Show a) => a -> a -> Map a [(a, Int)] -> Int
longestUPath start end g = go (S.singleton start) start
 where
  go seen cur
    | cur == end = 0
    | otherwise = maximumNI $ map (\(next, dist) -> dist + go (S.insert next seen) next) (filter (fst >>> (`S.notMember` seen)) (g !^ cur))

(!^) :: (Show k, Ord k) => Map k a -> k -> a
m !^ k = case M.lookup k m of
  Nothing -> error $ "Key " ++ show k ++ " not found"
  Just a -> a

maximumNI [] = -1000000000
maximumNI xs = maximum xs

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

type Coord = (Int, Int)
type Vec = Coord

(^+^) :: Vec -> Vec -> Vec
(r1, c1) ^+^ (r2, c2) = (r1 + r2, c1 + c2)

(^-^) :: Vec -> Vec -> Vec
(r1, c1) ^-^ (r2, c2) = (r1 - r2, c1 - c2)

above, below, left, right :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows)

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, left, right]

neighborsIn :: IArray UArray a => UArray Coord a -> Coord -> [Coord]
neighborsIn a = filter (inRange (bounds a)) . neighbors
