{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Lens
import           Control.Monad.Random hiding (next)
import           Data.Hashable
import qualified Data.HashSet         as HS
import           Data.List            (find, sortBy)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (fromJust)
import           Data.Ord             (comparing)

import           Data.Graph.AStar

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

type Loc = (Int,Int)

addLoc :: Loc -> Loc -> Loc
addLoc (x1,y1) (x2,y2) = (x1+x2, y1+y2)

------------------------------------------------------------
-- Nodes
------------------------------------------------------------

data Node = Node
  { _nodeLoc   :: Loc
  , _nodeUsed  :: Int
  , _nodeAvail :: Int
  }
  deriving Show

makeLenses ''Node

instance Eq Node where
  (Node l1 _ _) == (Node l2 _ _) = l1 == l2

nodeSize :: Node -> Int
nodeSize n = (n ^. nodeUsed) + (n ^. nodeAvail)

randomNode :: MonadRandom m => Loc -> m Node
randomNode loc = Node loc <$> getRandomR (0,100) <*> getRandomR (0,100)

readNodeName :: String -> Loc
readNodeName = (\[x,y] -> (x,y)) . map read . map tail . tail . splitOn "-"

readNode :: String -> Node
readNode
  = (\[n,_,u,a,_] -> Node (readNodeName n) (read . init $ u) (read . init $ a))
  . words

readInput :: String -> [Node]
readInput = map readNode . drop 2 . lines

------------------------------------------------------------
-- Part 1
------------------------------------------------------------

viable, viableRaw :: Node -> Node -> Bool
viableRaw nA nB = (nA ^. nodeUsed /= 0) && (nA ^. nodeUsed <= nB ^. nodeAvail)
viable nA nB = (nA /= nB) && viableRaw nA nB

-- Obvious, brute force solution.  O(n^2).
numViableBrute :: [Node] -> Int
numViableBrute ns = count (uncurry viable) [(a,b) | a <- ns, b <- ns]

-- Much better way to count viable pairs.  O(n lg n).  Sort list of
-- nodes twice: (1) by amount of used space, (2) by amount of
-- available space.  Use two-finger algorithm to count, and subtract
-- off overcounted nodes which are viable with themselves.
numViable :: [Node] -> Int
numViable ns = go byUsed (zip [len, len-1 ..] byAvail) - selfViable
  where
    len     = length ns

    -- Sort by used space, and drop any with 0 used space.
    byUsed  = dropWhile ((==0) . view nodeUsed) $ sortBy (comparing (view nodeUsed)) ns

    -- Sort by available space.
    byAvail = sortBy (comparing (view nodeAvail)) ns

    -- No more viable pairs if either list runs out.
    go [] _ = 0
    go _ [] = 0

    -- Check if the lowest remaining amount of used space fits in the
    -- lowest remaining available space.
    go (u:us) ((l,a):as)

      -- If so, node u makes a viable pair with everything in (a:as),
      -- so count its (previously cached) length l, and continue with
      -- the next lowest amount of space.
      | u ^. nodeUsed <= a ^. nodeAvail = l + go us ((l,a):as)

      -- If not, throw out the lowest available space (nothing else
      -- will fit on it) and try again.
      | otherwise = go (u:us) as

    -- Count nodes which would have been counted as viable pairs with
    -- themselves, so we can subtract them off.
    selfViable = count (\a -> viableRaw a a) ns

-- Huzzah, numViableBrute and numViable seem to agree on all the
-- random inputs I have tried!

------------------------------------------------------------
-- Part 2
------------------------------------------------------------

data PuzzleState = PS
  { _emptyLoc  :: Loc
  , _targetLoc :: Loc
  }
  deriving (Show, Eq, Ord)

makeLenses ''PuzzleState

instance Hashable PuzzleState where
  hashWithSalt s (PS l1 l2) = hashWithSalt s (l1,l2)

mkPuzzleState :: [Node] -> PuzzleState
mkPuzzleState ns = PS
  (view nodeLoc . fromJust $ find ((==0) . view nodeUsed) ns)
  (fst (gridSize ns), 0)

gridSize :: [Node] -> (Int, Int)
gridSize ns =
  ( fromJust $ maximumOf (traverse . nodeLoc . _1) ns
  , fromJust $ maximumOf (traverse . nodeLoc . _2) ns
  )

dirs :: [Loc]
dirs = [(0,1),(0,-1),(1,0),(-1,0)]

next :: Int -> Int -> HS.HashSet Loc -> PuzzleState -> HS.HashSet PuzzleState
next maxX maxY blockers (PS mt tg)
  = HS.fromList . map mkNext . filter valid $ [addLoc d mt | d <- dirs]
  where
    valid (x,y) = 0 <= x && x <= maxX && 0 <= y && y <= maxY
                  && not ((x,y) `HS.member` blockers)
    mkNext mt'  = if (mt' == tg) then PS mt' mt else PS mt' tg

solve :: [Node] -> Maybe [PuzzleState]
solve ns = aStar (next maxX maxY blockers) (\_ _ -> 1) dist goal (mkPuzzleState ns)
  where
    (maxX, maxY) = gridSize ns
    blockers = HS.fromList . map (^. nodeLoc) . filter ((>=500) . nodeSize) $ ns
    dist (PS _ (x,y)) = x + y
    goal (PS _ tg)    = tg == (0,0)

------------------------------------------------------------
-- Main
------------------------------------------------------------

main = do
  nodes <- readInput <$> getContents
  -- nodes <- evalRandIO (mapM randomNode [(x,y) | x <- [0..99], y <- [0..99]])

  print (numViable nodes)
  -- print (numViableBrute nodes)

  print (length . fromJust . solve $ nodes)

