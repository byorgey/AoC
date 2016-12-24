{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Arrow
import           Data.Array
import           Data.Graph.AStar
import           Data.Hashable
import qualified Data.HashSet     as HS
import           Data.List
import qualified Data.Map         as M
import           Data.Maybe
import qualified Data.Set         as S

-- Plan of attack:
--   (1) determine distances between each pair of numbers using bfs
--   (2) state = set of visited vertices + distance traveled
--   (3) do A* search to find shortest.

data Cell = Open | Blocked | Num Int
  deriving (Eq, Ord, Show)

type Dist = Int

isNum :: Cell -> Bool
isNum (Num _) = True
isNum _ = False

getNum (Num i) = i
getNum _ = error "o noes!"

type Maze = Array Loc Cell

readMaze :: String -> Maze
readMaze s = listArray ((0,0),(h-1,w-1)) . concatMap (map readCell) $ ls
  where
    ls = lines s
    w  = length (head ls)
    h  = length ls

readCell '.' = Open
readCell '#' = Blocked
readCell d   = Num (read [d])

dirs :: [Loc]
dirs = [(0,1),(0,-1),(1,0),(-1,0)]

distances :: Maze -> Loc -> [(Int, Dist)]
distances maze start = concatMap mkEntries . zip [1..] . tail $ numLocs
  where
    mkEntries (dist, locs) = map (\l -> (getNum (maze ! l), dist)) . S.toList $ locs
    numLocs = map (S.filter isNumLoc) bfsRes
    isNumLoc l = isNum (maze ! l)
    (_,(rmax,cmax)) = bounds maze
    bfsRes = bfs (const False) next (S.singleton start)
    next loc = S.fromList . filter valid $ [addLoc d loc | d <- dirs]
    valid (r,c) = 0 <= r && r <= rmax && 0 <= c && c <= cmax && maze ! (r,c) /= Blocked

allDistances :: Maze -> M.Map (Int,Int) Dist
allDistances maze = M.fromList $ concatMap numDistances nums
  where
    nums = (map . second) getNum . filter (isNum . snd) $ assocs maze
    numDistances (loc,n) = [((n,m), d) | (m,d) <- distances maze loc]

data SearchState = S { visited  :: [Int] -- keep sorted!
                     , cur      :: Int
                     , traveled :: Dist
                     }
  deriving (Eq, Ord, Show)

instance Hashable SearchState where
  hashWithSalt = hashUsing (\(S v c t) -> (v,c,t))

solve :: Bool -> Maze -> Maybe [SearchState]
solve ret maze = aStar next
               (\s1 s2 -> abs (traveled s1 - traveled s2))
               (\_ -> 0)
               (\s -> visited s == nums && (not ret || cur s == 0))
               (S [0] 0 0)
  where
    next :: SearchState -> HS.HashSet SearchState
    next (S vis c d)
      = HS.fromList
      $ map (\n -> S (sort . nub $ (n : vis)) n (d + dists M.! (c,n))) ((0 : (nums \\ vis)) \\ [c])
    dists = allDistances maze
    nums = sort . nub . map fst $ M.keys dists
    n = length nums

main = do
  maze <- readMaze <$> getContents

  -- print . solve False $ maze
  print . traveled . last . fromJust . solve False $ maze

  -- print . solve True $ maze
  print . traveled . last . fromJust . solve True $ maze

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
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

type Loc = (Int,Int)

addLoc :: Loc -> Loc -> Loc
addLoc (x1,y1) (x2,y2) = (x1+x2, y1+y2)
