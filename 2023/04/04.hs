#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package split

{-# LANGUAGE LambdaCase #-}

import Control.Category ((>>>))
import Data.List.Split (splitOn)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = [Card]
data Card = Card Int [Int] [Int] deriving (Eq, Show)

readInput :: String -> Input
readInput = lines >>> map readCard

readCard :: String -> Card
readCard =
  words >>> tail >>> splitOn ["|"] >>> \case
    [n : ws, hs] -> Card (read (init n)) (map read ws) (map read hs)

winning :: Card -> Int
winning (Card _ w h) = length (filter (`elem` w) h)

score :: Int -> Int
score 0 = 0
score n = 2 ^ (n - 1)

zipWithL _ [] ys = ys
zipWithL _ xs [] = xs
zipWithL f (x : xs) (y : ys) = f x y : zipWithL f xs ys

zipSum :: [[Int]] -> [Int]
zipSum [] = []
zipSum ([] : xss) = 0 : zipSum xss
zipSum ((x : xs) : xss) = x : zipWithL (+) xs (zipSum xss)

copies :: [Int] -> [Int]
copies ws = take (length ws) cs
 where
  cs = 1 : map succ (zipSum (zipWith replicate ws cs))

type Output = Int

solveA, solveB :: Input -> Output
solveA = map (winning >>> score) >>> sum
solveB = map winning >>> copies >>> sum
