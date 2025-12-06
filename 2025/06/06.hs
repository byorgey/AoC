#!/usr/bin/env stack
-- stack --resolver lts-24.21 script --package containers --package split --package array

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((>>>))
import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (splitWhen)

main =
  interact $
    applyAll [readInputA >>> solve, readInputB >>> solve]
      >>> map show
      >>> unlines

type Input = [Problem]
data Problem = Problem {numbers :: [Int], operation :: String}

------------------------------------------------------------
-- Parsing

readInputA :: String -> Input
readInputA = lines >>> map words >>> transpose >>> map readProblem
 where
  readProblem ws = Problem (map read (init ws)) (last ws)

readInputB :: String -> Input
readInputB (lines -> ls) = zipWith Problem nums ops
 where
  ops = words (last ls)
  nums = ls >$> init >>> transpose >>> splitWhen (all (== ' ')) >>> map (map (dropWhile isSpace >>> read))

------------------------------------------------------------
-- Evaluation

type Output = Int

solve :: Input -> Output
solve = map eval >>> sum

eval :: Problem -> Int
eval (Problem ns op) = foldl' (evalOp op) (opId op) ns
 where
  evalOp = \case "+" -> (+); "*" -> (*)
  opId = \case "+" -> 0; "*" -> 1

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
