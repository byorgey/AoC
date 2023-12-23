#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package linear --package lens --package mtl

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Lens (makeLenses, use, view, (%=), (-~), (.=), (^.))
import Control.Monad.State
import Data.List (find, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.Affine
import Linear.V3

------------------------------------------------------------

type P3 = Point V3
data Brick = Brick {loc :: P3 Int, axis :: V3 Int, len :: Int}
type Input = [Brick]

readInput :: String -> Input
readInput = lines >>> map readBrick >>> sortBy (comparing (view _z . loc))

readBrick :: String -> Brick
readBrick = splitOn "~" >>> map (splitOn "," >>> map read) >>> mkBrick
 where
  mkBrick [end1@[a, b, c], end2] = Brick (P (V3 a b c)) ax (l + 1)
   where
    Just (ax, l) = find (snd >>> (> 0)) (zip [V3 1 0 0, V3 0 1 0, V3 0 0 1] (zipWith (-) end2 end1)) <|> Just (V3 1 0 0, 0)

------------------------------------------------------------

expandBrick :: Brick -> Set (P3 Int)
expandBrick (Brick p v l) = S.fromList . take l $ iterate (.+^ v) p

support :: Set (P3 Int) -> Set (P3 Int)
support = S.map (_z -~ 1)

type BrickID = Int
type Blob = Map (P3 Int) BrickID

data BrickState = BS {_blob :: Blob, _supporters :: Map BrickID [BrickID], _supported :: Map BrickID [BrickID]}
  deriving (Show)

initBS :: BrickState
initBS = BS M.empty M.empty M.empty

makeLenses ''BrickState

dropBrick :: BrickID -> Brick -> Blob -> (Blob, [BrickID])
dropBrick bid b blob = go (expandBrick b)
 where
  go cells
    | any ((^. _z) >>> (== 0)) cells = (blob `M.union` M.fromSet (const bid) cells, [])
    | any (`M.member` blob) s = (blob `M.union` M.fromSet (const bid) cells, supportBricks)
    | otherwise = go s
   where
    s = support cells
    supportBricks = catMaybes . S.toList $ S.map (`M.lookup` blob) s

doDrop :: BrickID -> Brick -> State BrickState ()
doDrop bid b = do
  curBlob <- use blob
  let (blob', ss) = dropBrick bid b curBlob
  blob .= blob'
  supporters %= M.insert bid ss
  forM_ ss $ \s -> supported %= M.insertWith (++) s [bid]

dropBricks :: [Brick] -> BrickState
dropBricks bricks = execState (zipWithM_ doDrop [0 ..] bricks) initBS

disintegratable :: BrickState -> BrickID -> Bool
disintegratable (BS _ supporters supported) bid =
  all (\s -> length (fromMaybe [] (M.lookup s supporters)) > 1) (fromMaybe [] (M.lookup bid supported))

------------------------------------------------------------

-- Compute all the bricks that would fall if we remove a particular
-- brick.  Consider the "support graph" where bricks are vertices and
-- there is a directed edge from a to b if a directly supports b (note
-- this graph must be a DAG). Then for a given vertex b, we want to
-- know the maximal set of vertices F such that (1) there is a path
-- from b to each vertex in F, and (2) there is no path from any
-- vertex not in F to a vertex in F.  I don't know if there's a nice
-- graph theory term for that.  In any case we can find it using a
-- special BFS where at each step we cull any vertices which have
-- incoming edges from any vertices not already visited.
transitiveSupport :: BrickState -> BrickID -> Set BrickID
transitiveSupport (BS _ supporters supported) bid = bfs (S.singleton bid) (S.singleton bid)
 where
  bfs seen layer
    | S.null layer = seen
    | otherwise = bfs (S.union nextLayer seen) nextLayer
   where
    nextLayer = S.filter ok (foldMap next layer)
    next b = S.fromList . fromMaybe [] $ M.lookup b supported
    ok b = all (`S.member` seen) (fromMaybe [] (M.lookup b supporters))

countFalls :: BrickState -> Int
countFalls bs@(BS _ s _) = M.keys s >$> map (transitiveSupport bs >>> S.size >>> pred) >>> sum
 where
  bids = M.keys s

------------------------------------------------------------

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Output = Int

solveA, solveB :: Input -> Output
solveA bricks = count (disintegratable (dropBricks bricks)) bids
 where
  bids = [0 .. length bricks - 1]
solveB = dropBricks >>> countFalls

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
