#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

import           Control.Arrow        ((>>>))
import qualified Data.Foldable        as F
import           Data.List            (foldl1')
import           Data.Tuple           (swap)

import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

------------------------------------------------------------
-- Main

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Number]
data Number = Regular Int | Pair Number Number
  deriving (Eq, Show)

readInput :: String -> Input
readInput = lines >>> map readNumber

type Output = Int

solveA, solveB :: Input -> Output
solveA = foldl1' add >>> magnitude
solveB = pairs >>> concatMap (applyAll [id, swap]) >>>
  map (uncurry add >>> magnitude) >>> maximum

------------------------------------------------------------
-- Parsing

type Parser = Parsec Void String

readNumber :: String -> Number
readNumber = parse parseNumber "" >>> either undefined id

parseNumber :: Parser Number
parseNumber
  =   (Regular . read . (:[]) <$> digitChar)
  <|> (Pair <$> ("[" *> parseNumber) <*> ("," *> parseNumber <* "]"))

------------------------------------------------------------
-- Number zipper

data Tooth = L Number | R Number deriving (Eq, Show)
data Zipper = Number :< [Tooth]

open :: Number -> Zipper
open n = n :< []

left :: Zipper -> Zipper
left z@(Regular{} :< _) = z
left (Pair l r :< ts)   = l :< (R r : ts)

right :: Zipper -> Zipper
right z@(Regular{} :< _) = z
right (Pair l r :< ts)   = r :< (L l : ts)

up :: Zipper -> Zipper
up (n :< [])         = n :< []
up (n :< (L l : ts)) = Pair l n :< ts
up (n :< (R r : ts)) = Pair n r :< ts

focus :: Zipper -> Number
focus (n :< _) = n

onRightmost :: (Int -> Int) -> Number -> Number
onRightmost f (Regular n) = Regular (f n)
onRightmost f (Pair l r)  = Pair l (onRightmost f r)

onLeftmost :: (Int -> Int) -> Number -> Number
onLeftmost f (Regular n) = Regular (f n)
onLeftmost f (Pair l r)  = Pair (onLeftmost f l) r

------------------------------------------------------------
-- Snailfish numbers

magnitude :: Number -> Int
magnitude (Regular n) = n
magnitude (Pair l r)  = 3 * magnitude l + 2 * magnitude r

add :: Number -> Number -> Number
add x y = reduce (Pair x y)

reduce :: Number -> Number
reduce = exhaust (F.asum . applyAll [explode, split])

exhaust :: (a -> Maybe a) -> a -> a
exhaust f = go
  where
    go a = case f a of
      Just a' -> go a'
      Nothing -> a

explode :: Number -> Maybe Number
explode = fmap doExplosion . findExploder 0 . open
  where
    findExploder :: Int -> Zipper -> Maybe (Int, Int, [Tooth])
    findExploder _ (focus -> Regular _) = Nothing
    findExploder !n (Pair (Regular l) (Regular r) :< ts)
      | n >= 4 = Just (l, r, ts)
    findExploder n z = findExploder (n+1) (left z) <|> findExploder (n+1) (right z)

    doExplosion :: (Int, Int, [Tooth]) -> Number
    doExplosion (l, r, ts) = rezip (Just l) (Just r) (Regular 0 :< ts)

    rezip :: Maybe Int -> Maybe Int -> Zipper -> Number
    rezip _ _ (n :< []) = n
    rezip (Just l) r (n :< (L tl : ts)) = rezip Nothing r (Pair (onRightmost (+l) tl) n :< ts)
    rezip l (Just r) (n :< (R tr : ts)) = rezip l Nothing (Pair n (onLeftmost (+r) tr) :< ts)
    rezip l r z = rezip l r (up z)

split :: Number -> Maybe Number
split (Regular x)
  | x >= 10 = Just (Pair (Regular (x `div` 2)) (Regular ((x+1) `div` 2)))
  | otherwise = Nothing
split (Pair l r)
  = (Pair <$> split l <*> pure r) <|> (Pair <$> pure l <*> split r)

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs
