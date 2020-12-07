{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           Numeric

main = interact $
  lines >>> map readSeat >>> applyAll [solveA, solveB] >>> map show >>> unlines

type Seat = (Int,Int)

seatID :: Seat -> Int
seatID (r,c) = r * 8 + c

readSeat :: String -> Seat
readSeat = splitAt 7 >>> (readBin "FB" *** readBin "LR")

readBin :: [Char] -> String -> Int
readBin bits = readInt 2 (`elem` bits) (\c -> fromJust (findIndex (==c) bits)) >>> head >>> fst

solveA, solveB :: [Seat] -> Int
solveA = map seatID >>> maximum
solveB = map seatID >>> sort >>> (zip <*> tail) >>>
  find (uncurry subtract >>> (==2)) >>> fromJust >>> fst >>> succ

applyAll fs x = map ($x) fs

