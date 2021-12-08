{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE TupleSections      #-}

import           Control.Applicative  (liftA2)
import           Control.Arrow        ((>>>))
import           Data.Functor.Compose
import           Data.List            (find, findIndex, foldl')
import           Data.List.Split      (splitOn)
import           Data.Maybe           (fromJust)
import           Data.Set             (Set)
import qualified Data.Set             as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[[Set Char]]]

readInput :: String -> Input
readInput = lines >>> map (words >>> splitOn ["|"] >>> map (map S.fromList))

type Output = Int

solveA, solveB :: Input -> Output
solveA = map (last >>> count (S.size >>> (`elem` [2,3,4,7]))) >>> sum
solveB = map solveB1 >>> sum

solveB1 :: [[Set Char]] -> Int
solveB1 [signals, code] = foldl' (\n d -> 10*n + d) 0 (map decode code)
  where
    decode c = fromJust (findIndex (==c) digits)
    digits = solveForDigits signals

------------------------------------------------------------

-- This is a kind of generalized spreadsheet evaluation function.
-- See https://github.com/quchen/articles/blob/master/loeb-moeb.md .
löb :: Functor f => f (f a -> a) -> f a
löb g = f where f = fmap ($f) g

-- Just write down a logical characterization of each digit in a
-- spreadsheet (possibly in terms of other digits in the
-- spreadsheet!), and run it to find which set of segments corresponds
-- to each digit.  Laziness ensures they will be computed in the
-- correct order when deducing one digit depends on already having
-- deduced another one.
solveForDigits :: [Set Char] -> [Set Char]
solveForDigits inputs = löb . map (\p ds -> find' (p ds) inputs) $ spreadsheet
  where
    spreadsheet :: [[Set Char] -> Set Char -> Bool]
    spreadsheet = map unDigital
      [ size .==. 6 .&&. inCommonWith 1 .==. 2 .&&. inCommonWith 4 .==. 3
      , size .==. 2
      , size .==. 5 .&&. inCommonWith 4 .==. 2
      , size .==. 5 .&&. inCommonWith 4 .==. 3 .&&. inCommonWith 1 .==. 2
      , size .==. 4
      , size .==. 5 .&&. inCommonWith 4 .==. 3 .&&. inCommonWith 1 .==. 1
      , size .==. 6 .&&. inCommonWith 1 .==. 1
      , size .==. 3
      , size .==. 7
      , size .==. 6 .&&. inCommonWith 1 .==. 2 .&&. inCommonWith 4 .==. 4
      ]

------------------------------------------------------------
-- A little DSL for expressing things about digits.

-- A 'Digital a' is a computation of a value of type 'a', with access
-- to the list of segment sets corresponding to each digit and a
-- particular, focused set of segments.
newtype Digital a = Digital { unDigital :: [Set Char] -> Set Char -> a }
  deriving Functor
  deriving Applicative via Compose ((->) [Set Char]) ((->) (Set Char))

-- Giving an incomplete Num instance is just a hack to allow us to write
-- a literal integer value to represent a Digital Int.
instance Num a => Num (Digital a) where
  fromInteger = pure . fromInteger

-- digit d gets the set of segments which we know corresponds to the digit d.
digit :: Int -> Digital (Set Char)
digit d = Digital $ \ds _ -> ds!!d

-- Get the focused set of segments.
theDigit :: Digital (Set Char)
theDigit = Digital $ \_ d -> d

-- Get the number of segments in the focused set.
size :: Digital Int
size = S.size <$> theDigit

-- Lifted equality and logical and.
infix 4 .==.
(.==.) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(.==.) = liftA2 (==)

infixr 3 .&&.
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

-- Check how many segments two sets have in common.
inCommon :: Ord a => Set a -> Set a -> Int
inCommon s1 s2 = S.size (s1 `S.intersection` s2)

-- Check how many segments the focused set has in common with the set
-- of segments corresponding to a particular number.
inCommonWith :: Int -> Digital Int
inCommonWith d = inCommon <$> theDigit <*> digit d

------------------------------------------------------------
-- Some simple utilities.

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust
