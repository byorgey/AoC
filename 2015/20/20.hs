{-# LANGUAGE LambdaCase #-}

import           Control.Arrow
import           Data.Bits
import           Data.IntSet                           (elems)
import           Data.List
import           Data.MemoTrie
import qualified Math.NumberTheory.ArithmeticFunctions as NT

penta :: Integer -> Integer
penta n = n*(3*n-1) `div` 2

sigma :: Integer -> Integer
sigma = memo sigma'
  where
    sigma' :: Integer -> Integer
    sigma' n
      | n <= 0 = 0
      | otherwise
      = sum . zipWith ($) (cycle [id,id,negate,negate])
      . map (\case { 0 -> n; i -> sigma i })
      . takeWhile (>= 0) . map (n-) $ genPenta
    genPenta :: [Integer]
    genPenta = map penta ([1..] >>= (\x -> [x,-x]))

main = do
--  mapM_ print (map (id &&& sigma) [0, 100 .. 600000])
  print (find ((>= 2900000) . NT.sigma 1) [1 :: Int .. 2900000])

-- Sigh, in the end the right answer was just using a properly
-- optimized library.

  print (find ((>= 29000000) . elfValue2) [1 :: Int .. 29000000])

elfValue2 n = 11 * (NT.sigma 1 n - sum (takeWhile (\d -> n `div` d > 50) (elems (NT.divisorsSmall n))))

-- Notes from earlier attempts:

------------------------------------------------------------
-- Looking for smallest n such that sigma(n) = 2900000.
--
-- Note sigma(n) is multiplicative; does that help?
-- 2900000 = 2^5 * 5^5 * 29.
-- Note there is no n for which sigma(n) = 29.
--
-- Record sigma(n) is powers of 2?  No, not even.
--
-- Note n+1 <= sigma(n) <= (n^2 + n)/2 obviously.
-- Apparently (wikipedia, Robin's Theorem)
--
--   sigma(n) < e^gamma log log n
--
-- for n > 5040 if Riemann Hypothesis.
--
-- So we have
--
-- n+1 <= 2900000 < e^gamma n log log n
-- so n <= 2900000 - 1 and also
--
--   2900000 / e^gamma < n log log n
--
-- approx. n > 600000.
-- so n is between 0.6 million and 2.9 million.
--
-- I've only succeeded in computing sigma up to 100k and that took a
-- while.  Might as well start it searching above 600k, maybe I'll get
-- lucky.  Oh, but to compute sigma(600k) it first has to compute it
-- for all < that.
--
-- Maybe do a hybrid method where for small enough n we compute it
-- based on factorization, and for larger n use the recurrence?  Don't
-- know if that would help or be faster at all.
--

----------------------------------------------------------------------
-- Didn't end up actually needing the stuff below
----------------------------------------------------------------------

isqrt :: Integer -> Maybe Integer
isqrt n = go n
  where
    go s
      | s * s == n = Just s
      | s * s < n  = Nothing
      | otherwise  = go ((s + n `div` s) `div` 2)

isGenPentagonal :: Integer -> Maybe Integer
isGenPentagonal n =
  case isqrt (1 + 24*n) of
    Nothing -> Nothing
    Just s  -> case (s `mod` 6) of
      5 -> Just $ (1 + s) `div` 6
      1 -> Just $ (1 - s) `div` 6
      _ -> Nothing

{-
      3i^2 - i = 2n
      3i^2 - i - 2n = 0

      (1 +/- sqrt(1 + 24n)) / 6 = i   must be an integer.
      <=>
      1 +/- sqrt(1 + 24n) is divisible by 6.
      <=>
      +/- sqrt(1 + 24n) == 5 mod 6.
-}
