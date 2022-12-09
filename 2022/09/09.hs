#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package mtl

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.Set            (Set)
import qualified Data.Set            as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Move]
data Move = Move { dir :: Dir, cnt :: Int } deriving (Eq, Show)
data Dir = R | L | D | U deriving (Eq, Read, Show)

readInput :: String -> Input
readInput = lines >>> map (words >>> \[d,n] -> Move (read d) (read n))

type Output = Int

solveA, solveB :: Input -> Output
solveA = mapM_ execMove >>> flip execState (initS 2) >>> vis >>> S.size
solveB = mapM_ execMove >>> flip execState (initS 10) >>> vis >>> S.size

type Coord = (Int,Int)

move :: Dir -> Coord -> Coord
move R (x,y) = (x+1,y)
move L (x,y) = (x-1,y)
move D (x,y) = (x,y-1)
move U (x,y) = (x,y+1)

follow :: Coord -> Coord -> Coord
follow (hx,hy) t@(tx,ty)
  | abs (hx-tx) > 1 || abs (hy-ty) > 1 = (tx + signum (hx-tx), ty + signum (hy-ty))
  | otherwise = t

moves :: Dir -> [Coord] -> [Coord]
moves d (c:cs) = go (move d c : cs)
  where
    go [h]      = [h]
    go (h:t:ts) = h : go (follow h t : ts)

data S = S { rope :: [Coord], vis :: Set Coord }

initS :: Int -> S
initS n = S (replicate n (0,0)) (S.singleton (0,0))

execMove :: Move -> State S ()
execMove (Move d n) = replicateM_ n (execDir d)

execDir :: Dir -> State S ()
execDir d = do
  S r v <- get
  let r' = moves d r
  put $ S r' (S.insert (last r') v)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
