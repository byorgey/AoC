#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package mtl --package array

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Arrow       ((&&&), (>>>))
import           Control.Monad.State
import           Data.Array.Unboxed
import           Data.Bool           (bool)
import           Data.Function       (on)
import           Data.List           (groupBy)
import           Data.Map            (Map, (!))
import qualified Data.Map.Strict     as M
import           Data.Maybe          (mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Output = Int

solveA, solveB :: Input -> Output
solveA = initSt >>> execState (replicateM_ 10 step) >>> elves >>> emptyGround
solveB = initSt >>> execState stepUntilSteady >>> roundCount

------------------------------------------------------------
-- Input

type Input = Set Coord

readInput :: String -> Input
readInput = lines >>> map (map (=='#')) >>> mkArray >>> assocs >>> filter snd >>> map fst >>> S.fromList

------------------------------------------------------------
-- Simulation

consider :: [[Coord -> Coord]]
consider =
  [ [above, above.left, above.right]
  , [below, below.left, below.right]
  , [left, left.above, left.below]
  , [right, right.above, right.below]
  ]

data St = St { considerIndex :: Int, roundCount :: Int, elves :: Set Coord }

initSt :: Set Coord -> St
initSt = St 0 0

step :: State St Bool
step = do
  St i r es <- get
  let withProposals = map (id &&& propose i es) (S.toList es)
      proposals = cardinality (mapMaybe snd withProposals)
      dests = map pick withProposals

      pick (l, Nothing) = l
      pick (l, Just p) = case M.lookup p proposals of
        Just 1 -> p
        _      -> l

  let es' = S.fromList dests
  put (St ((i+1) `mod` 4) (r+1) es')
  return (es == es')

propose :: Int -> Set Coord -> Coord -> Maybe Coord
propose i es c
  | all (`S.notMember` es) (neighbors8 c) = Nothing
  | otherwise = msum (map try considerations)
  where
    considerations = map ((consider!!) . (`mod`4)) [i .. i+3]
    try dirs
      | any (($ c) >>> (`S.member` es)) dirs = Nothing
      | otherwise = Just (head dirs c)

------------------------------------------------------------
-- Debugging

showElves :: Set Coord -> String
showElves es = unlines $ map (map ((`S.member` es) >>> bool '.' '#')) cs
  where
    r = rect es
    cs = groupBy ((==) `on` fst) (range r)

------------------------------------------------------------
-- Part 1

rect :: Set Coord -> (Coord, Coord)
rect cs = ((minimum rows, minimum cols), (maximum rows, maximum cols))
  where
    (rows, cols) = unzip (S.toList cs)

emptyGround :: Set Coord -> Int
emptyGround cs = count (`S.notMember` cs) (range (rect cs))

------------------------------------------------------------
-- Part 2

stepUntilSteady :: State St ()
stepUntilSteady = do
  steady <- step
  unless steady stepUntilSteady

------------------------------------------------------------
-- Utilities

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

cardinality :: Ord a => [a] -> Map a Int
cardinality = map (,1) >>> M.fromListWith (+)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Coord = (Int,Int)

above, below, left, right :: Coord -> Coord
above (r,c) = (r-1,c)
below (r,c) = (r+1,c)
left (r,c) = (r,c-1)
right (r,c) = (r,c+1)

mkArray :: IArray UArray a => [[a]] -> UArray Coord a
mkArray rows = listArray ((0,0), (length rows - 1, length (head rows) - 1)) (concat rows)

neighbors8 :: Coord -> [Coord]
neighbors8 = applyAll [above, above.left, above.right, left, right, below, below.left, below.right]
