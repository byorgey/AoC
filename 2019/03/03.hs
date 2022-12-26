#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow   ((>>>))
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

data Dir = U | D | L | R deriving (Eq, Ord, Read, Show, Enum, Bounded)
data Step = Step Dir Int
type Path = [Step]
type Input = [Path]

readInput :: String -> Input
readInput = lines >>> map (splitOn "," >>> map readStep)

readStep :: String -> Step
readStep (d:n) = Step (read [d]) (read n)

------------------------------------------------------------

expandStep :: Step -> Coord -> [Coord]
expandStep (Step d n) = iterate (move d) >>> drop 1 >>> take n

move :: Dir -> (Coord -> Coord)
move = fromEnum >>> ([above, below, left, right]!!)

expandPath :: Path -> Map Coord Int
expandPath = go (0,0) 0
  where
    go _ _ [] = M.empty
    go c t (s@(Step _ n):ss) = M.fromList (zip cs [t+1 ..]) `M.union` go (last cs) (t + n) ss
      where
        cs = expandStep s c

dist :: Coord -> Int
dist (x,y) = abs x + abs y

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA = map expandPath >>> uncurryL M.intersection >>> M.keys >>> map dist >>> minimum
solveB = map expandPath >>> uncurryL (M.intersectionWith (,)) >>> M.elems >>> map (uncurry (+)) >>> minimum

------------------------------------------------------------

uncurryL :: (a -> a -> b) -> [a] -> b
uncurryL f [x,y] = f x y

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Coord = (Int,Int)

above, below, left, right :: Coord -> Coord
above (r,c) = (r-1,c)
below (r,c) = (r+1,c)
left (r,c) = (r,c-1)
right (r,c) = (r,c+1)
