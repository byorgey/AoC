#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((>>>))
import Data.Bits
import Data.List (mapAccumL)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = [[Int]]

readInput :: String -> Input
readInput = lines >>> map (map (\case '.' -> 0; _ -> 1))

type Beams = Integer
type Splitters = Integer
type Output = Int

toBits :: [Int] -> Splitters
toBits = foldl' (\n b -> (n `shiftL` 1) .|. fromIntegral b) 0

solveA, solveB :: Input -> Output
solveA (map toBits -> (start : ss)) = ss >$> mapAccumL split start >>> snd >>> sum
 where
  split :: Beams -> Splitters -> (Beams, Int)
  split beams splitters = (beams', popCount splits)
   where
    splits = beams .&. splitters
    beams' = (beams `xor` splits) .|. (splits `shiftL` 1) .|. (splits `shiftR` 1)
solveB (start : ss) = sum $ foldl' split start ss
 where
  split :: [Int] -> [Int] -> [Int]
  split beams splitters = beams'
   where
    (.+.) = zipWith (+)
    splits = zipWith (*) beams splitters
    beams' = zipWith (-) beams splits .+. (0 : splits) .+. (tail splits ++ [0])

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
