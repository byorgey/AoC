{-# LANGUAGE TupleSections #-}

import           Control.Arrow
import           Data.List
import           Data.Maybe

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> unlines

type Input = [String]

readInput :: String -> Input
readInput = lines

type Output = String

solveA, solveB :: Input -> Output
solveA ids = show $ count (has 2) ids * count (has 3) ids
  where
    has n = sort >>> group >>> any (length >>> (==n))
solveB ids = ids >$> pairs >>> map (uncurry zap) >>> find (length >>> (==n-1)) >>> fromJust
  where
    n = length (head ids)
    zap x y = map fst . filter (uncurry (==)) $ zip x y

------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
