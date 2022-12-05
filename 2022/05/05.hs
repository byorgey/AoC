#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package mtl --package lens

import           Control.Arrow       ((>>>))
import           Control.Lens        (ix, use, (%=))
import           Control.Monad.State
import           Data.Char           (isAlpha, isDigit, isSpace)
import           Data.List           (transpose)
import           Data.List.Split     (splitOn)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

-- Input ---------------------------------------------------

data Input = Input { stacks :: [[Char]], procedure :: [Move] } deriving (Eq, Show)
data Move = Move { number :: Int, from :: Int, to :: Int } deriving (Eq, Show)

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> \[ss,ms] -> Input (readStacks ss) (readMoves ms)

readStacks :: [String] -> [[Char]]
readStacks = init >>> transpose >>> filter (any isAlpha) >>> map (dropWhile isSpace)

readMoves :: [String] -> [Move]
readMoves = map (words >>> filter (all isDigit) >>> map read >>> mkMove)
  where
    mkMove [n,f,t] = Move n f t

-- Solving -------------------------------------------------

type Output = String

solveA, solveB :: Input -> Output
solveA = solve True
solveB = solve False

solve :: Bool -> Input -> Output
solve shouldReverse (Input stks ms) = map head $ execState (mapM_ (execMove shouldReverse) ms) stks

execMove :: Bool -> Move -> State [[Char]] ()
execMove shouldReverse (Move n f t) = do
  cs <- take n <$> use (ix (f-1))
  ix (f-1) %= drop n
  ix (t-1) %= ((if shouldReverse then reverse else id) cs ++)

