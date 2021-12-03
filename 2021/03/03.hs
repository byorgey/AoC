{-# LANGUAGE TupleSections #-}

import           Control.Arrow
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust)
import           Data.Ord
import qualified Numeric       as N

main = interact $
  lines >>> applyAll [solveA,solveB] >>> map show >>> unlines

solveA, solveB :: [String] -> Int
solveA = transpose >>> applyAll [map oxygen, map co2] >>> map readBin >>> product
solveB = applyAll [rating oxygen, rating co2] >>> map readBin >>> product

rating :: ([Char] -> Char) -> [String] -> String
rating r [x] = x
rating r xs = (filter (head >>> (==b)) >>> map tail >>> rating r >>> (b:)) xs
  where
    b = r (map head xs)

oxygen = substance (>)
co2    = substance (<=)

substance zeroRule bs
  | zeroRule zs os = '0'
  | otherwise = '1'
  where
    (zs, os) = (length *** length) $ partition (=='0') bs

readBase :: Int -> [Char] -> String -> Int
readBase b digits s =
  case N.readInt b (`elem` digits) (flip elemIndex digits >>> fromJust) s of
    ((a,_):_) -> a

readBin = readBase 2 "01"

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
