#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Arrow ((>>>))
import           Data.Char     (ord)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = [[String]]

data RPS = R|P|S deriving (Eq, Ord, Show, Enum, Bounded)
data Outcome = L|D|W deriving (Eq, Ord, Show, Enum, Bounded)

readInput :: String -> Input
readInput = lines >>> map words

readRPS [c] = [R,P,S]!!i
  where
    i
      | c <= 'C' = ord c - ord 'A'
      | otherwise = ord c - ord 'X'

readOutcome [c] = [L,D,W]!!(ord c - ord 'X')

type Output = Int

solveA, solveB :: Input -> Output
solveA = map (map readRPS >>> scoreRound) >>> sum
solveB = map (strategy >>> scoreRound) >>> sum

scoreRound [a,b] = fromEnum b + 1 + winScore a b

winScore a b = ((fromEnum b - fromEnum a + 1) `mod` 3) * 3

strategy [rS,oS] = [r, play]
  where
    r = readRPS rS
    o = readOutcome oS

    play = toEnum $ (fromEnum r + (fromEnum o - 1)) `mod` 3
