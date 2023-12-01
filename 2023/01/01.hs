#!/usr/bin/env stack
-- stack --resolver lts-21.22 script

{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow (first, (&&&), (>>>))
import Data.Char (isDigit)
import Data.List (find, isPrefixOf)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

type Input = [String]

readInput :: String -> Input
readInput = lines

type Output = Int

solveA, solveB :: Input -> Output
solveA = map (filter isDigit >>> (head &&& last) >>> (\(a, b) -> read [a, b])) >>> sum
solveB = map ((firstDigit &&& lastDigit) >>> (\(a, b) -> read [a, b])) >>> sum

digits :: [(String, Char)]
digits = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ['1' ..]

findDigit :: [(String, Char)] -> String -> Char
findDigit ds s@(x : xs)
  | isDigit x = x
  | Just (_, d) <- find (\(digit, _) -> digit `isPrefixOf` s) ds = d
  | otherwise = findDigit ds xs

firstDigit = findDigit digits
lastDigit = reverse >>> findDigit (map (first reverse) digits)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
