{-# LANGUAGE TupleSections #-}

import           Control.Arrow
import           Data.Array.Unboxed
import           Data.List
import           Data.Maybe

main = interact $
  readInput >>> applyAll [solveA, solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = words >>> map read

type Output = Int

preamble = 25

solveA, solveB :: Input -> Output
solveA xs = zip (map (take preamble) (tails xs)) (drop preamble xs) >$>
  find (not . uncurry isSumOf) >>> fromJust >>> snd

solveB xs = minimum rng + maximum rng
  where
    s :: UArray Int Int
    s = listArray (0, length xs) (scanl (+) 0 xs)

    magicNumber = solveA xs

    (i,j) = fromJust $ find (\(i,j) -> s!j - s!i == magicNumber) (pairs [0 .. length xs])

    rng = drop i (take j xs)

isSumOf :: [Int] -> Int -> Bool
isSumOf xs y = any (uncurry (+) >>> (==y)) (pairs xs)

------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
