#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

{-# LANGUAGE ImportQualifiedPost #-}

import Control.Category ((>>>))
import Data.Bits ((.&.))
import Data.Char (isAlpha, ord)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (deleteBy, foldl')
import Data.List.Split (splitOn)

------------------------------------------------------------
-- Input

type Input = [String]

readInput :: String -> Input
readInput = lines >>> head >>> splitOn ","

type Label = String
data Op = Remove | Insert Int deriving (Eq, Show)
data Step = Step Label Op deriving (Eq, Show)

readStep :: String -> Step
readStep step = Step label op
 where
  (label, opcode) = span isAlpha step
  op = case opcode of
    "-" -> Remove
    '=' : fl -> Insert (read fl)

------------------------------------------------------------
-- Solution

hash :: String -> Int
hash = foldl' (\h x -> (17 * (h + ord x)) .&. 0xFF) 0

type HASHMAP = IntMap [(Label, Int)]

emptyHASHMAP :: HASHMAP
emptyHASHMAP = IM.fromList [(i, []) | i <- [0 .. 255]]

execStep :: HASHMAP -> Step -> HASHMAP
execStep hm (Step l op) = IM.adjust upd (hash l) hm
 where
  upd = case op of
    Remove -> deleteBy ((==) `on` fst) (l, 0)
    Insert n -> insert l n
  insert l n [] = [(l, n)]
  insert l n ((l', n') : ls)
    | l == l' = (l, n) : ls
    | otherwise = (l', n') : insert l n ls

focusingPower :: HASHMAP -> Int
focusingPower = IM.assocs >>> concatMap focusBox >>> sum
 where
  focusBox :: (Int, [(Label, Int)]) -> [Int]
  focusBox (i, ls) = zipWith (\p (_, n) -> p * n * (i + 1)) [1 ..] ls

------------------------------------------------------------
-- Main

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Output = Int

solveA, solveB :: Input -> Output
solveA = map hash >>> sum
solveB = map readStep >>> foldl' execStep emptyHASHMAP >>> focusingPower
