#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

{-# LANGUAGE LambdaCase #-}

import           Control.Arrow   ((>>>))
import           Data.List       (scanl')
import           Data.List.Split (chunksOf)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> unlines

------------------------------------------------------------

type Input = [Instruction]
data Instruction = Noop | AddX Int deriving (Eq, Show)

readInput :: String -> Input
readInput = lines >>> map readInstruction

readInstruction = words >>> \case
  ["noop"]    -> Noop
  ["addx", n] -> AddX (read n)

------------------------------------------------------------

type Output = String

solveA, solveB :: Input -> Output
solveA = registerValues >>> applyAll (map signalStrength [20,60,100,140,180,220]) >>> sum >>> show
solveB = registerValues >>> drop 1 >>> zipWith draw [1..240] >>> chunksOf 40 >>> unlines

registerValues :: [Instruction] -> [Int]
registerValues = concatMap (\case {Noop -> [0]; AddX n -> [0,n]}) >>> scanl' (+) 1 >>> (0:)

signalStrength i xs = i * (xs !! i)

draw :: Int -> Int -> Char
draw c x = if abs (x - (c-1) `mod` 40) <= 1 then '#' else '.'

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
