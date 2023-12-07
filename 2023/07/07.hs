#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Category ((>>>))
import Data.Function (on)
import Data.List (sortBy, sortOn)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..), comparing)

main =
    interact
        $ readInput
        >>> applyAll [solveA, solveB]
        >>> map show
        >>> unlines

------------------------------------------------------------
-- Reading input

type Bid = Int
data Card = N Int | T | J | Q | K | A deriving (Eq, Read, Ord, Show)
newtype Hand = Hand {handCards :: [Card]} deriving (Eq, Show)
type Input = [(Hand, Bid)]

readInput :: String -> Input
readInput = lines >>> map (words >>> \[h, b] -> (readHand h, read b))

readHand :: String -> Hand
readHand = map readCard >>> Hand

readCard :: Char -> Card
readCard c
    | c `elem` "AKQJT" = read [c]
    | otherwise = N (read [c])

------------------------------------------------------------
-- Hand comparison

-- The hand type of a hand is just the list of card counts, sorted in
-- descending order.  For example, a full house has hand type [3,2].
-- Comparing these lexicographically gives the correct ordering.
type HandType = [Int]

handType :: Hand -> HandType
handType = handCards >>> cardinality >>> M.elems >>> sortOn Down

handTypeWithJokers :: Hand -> HandType
handTypeWithJokers (Hand cs)
    | numJokers == 5 = [5]
    | otherwise = otherCounts >$> onHead (+ numJokers)
  where
    numJokers = count (== J) cs
    otherCounts = cs >$> filter (/= J) >>> cardinality >>> M.elems >>> sortOn Down

instance Ord Hand where
    compare = comparing handType <> comparing handCards

compareJokerHands :: Hand -> Hand -> Ordering
compareJokerHands = comparing handTypeWithJokers <> comparing (handCards >>> map demoteJoker)
  where
    demoteJoker J = N 1
    demoteJoker c = c

type Output = Int

scoreBids :: [Int] -> Int
scoreBids = zip [1 ..] >>> map (uncurry (*)) >>> sum

solve :: (Hand -> Hand -> Ordering) -> Input -> Output
solve cmp = sortBy (cmp `on` fst) >>> map snd >>> scoreBids

solveA, solveB :: Input -> Output
solveA = solve compare
solveB = solve compareJokerHands

------------------------------------------------------------
-- Utilities

infixr 0 >$>
(>$>) = flip ($)

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

cardinality :: Ord a => [a] -> Map a Int
cardinality = map (,1) >>> M.fromListWith (+)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (a : as) = f a : as
