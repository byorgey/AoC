{-# LANGUAGE TupleSections #-}

import           Control.Arrow
import           Data.Foldable
import           Data.List
import           Data.Maybe

main = interact $
  words >>> map read >>> sort >>> solveB >>> show >>> (++"\n")

solveA :: [Integer] -> Integer
solveA = uncurry (*) . fromJust . sumTo 2020

solveB :: [Integer] -> Integer
solveB xs = x*y*z
  where
    Just (x, (y,z)) = asum (map (\x -> (x,) <$> sumTo (2020-x) xs) xs)

-- list must already be sorted
sumTo :: Integer -> [Integer] -> Maybe (Integer, Integer)
sumTo target ys = go ys (reverse ys)
  where
    go [] _ = Nothing
    go _ [] = Nothing
    go (x:xs) (y:ys)
      | s == target = Just (x,y)
      | s < target  = go xs (y:ys)
      | otherwise   = go (x:xs) ys
      where
        s = x + y

