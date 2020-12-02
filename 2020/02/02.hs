{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow
import           Data.List.Split

main = interact $
  lines >>> map (words >>> readPassword) >>> (count validA &&& count validB) >>> format

format (n1,n2) = unlines [show n1, show n2]

data Password = P { lo :: Int, hi :: Int, keyChar :: Char, password :: String }

readPassword :: [String] -> Password
readPassword [rng,k,pw] = P lo hi (head k) pw
  where
    [lo,hi] = map read (splitOn "-" rng)

validA :: Password -> Bool
validA (P{..}) = lo <= cnt && cnt <= hi
  where
    cnt = count (==keyChar) password

validB :: Password -> Bool
validB (P{..}) = count (==keyChar) [password!!(lo-1), password!!(hi-1)] == 1

count p = filter p >>> length
