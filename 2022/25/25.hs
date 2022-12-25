#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

import           Control.Applicative
import           Control.Arrow       ((>>>))
import           Data.List

main = interact $ lines >>> solve

fromSNAFU :: String -> Int
fromSNAFU = foldl' (\n d -> 5*n + snafu2Int d) 0

snafu2Int :: Char -> Int
snafu2Int '-' = -1
snafu2Int '=' = -2
snafu2Int d   = read [d]

bmod5 :: Int -> Int
bmod5 n
  | m > 2 = m - 5
  | otherwise = m
  where m = n `mod` 5

int2Snafu :: Int -> Char
int2Snafu (-2) = '='
int2Snafu (-1) = '-'
int2Snafu n    = head (show n)

toSNAFU :: Int -> String
toSNAFU = reverse . go
  where
    go 0 = ""
    go n = int2Snafu d : go ((n - d) `div` 5)
      where d = bmod5 n

solve :: [String] -> String
solve = map fromSNAFU >>> sum >>> toSNAFU
