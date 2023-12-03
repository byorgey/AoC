#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Category ((>>>))
import Data.Char (isDigit)
import Data.List (nub)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

data Number = N {value :: Int, start :: Coord, end :: Coord} deriving (Show, Eq)
data Symbol = S {symbol :: Char, loc :: Coord} deriving (Show)
data Notation = Number Number | Symbol Symbol deriving (Show)
type Input = [Notation]

readInput :: String -> Input
readInput = lines >>> zip [0 ..] >>> concatMap readSchematicLine

readSchematicLine :: (Int, String) -> [Notation]
readSchematicLine (r, l) = go (zip [0 ..] l)
 where
  go [] = []
  go ((_, '.') : xs) = go xs
  go xs@((c, d) : ys)
    | isDigit d =
        let (ds, zs) = span (snd >>> isDigit) xs
         in Number (N (read (map snd ds)) (r, c) (r, fst (last ds))) : go zs
    | otherwise = Symbol (S d (r, c)) : go ys

type WithCoords a = ([a], Map Coord a)

class HasCoords a where
  coords :: a -> [Coord]

  withCoords :: a -> WithCoords a
  withCoords a = ([a], M.fromList (map (,a) (coords a)))

emptyWC :: WithCoords a
emptyWC = ([], M.empty)

instance HasCoords Number where
  coords (N _ (r, s) (_, e)) = [(r, c) | c <- [s .. e]]

instance HasCoords Symbol where
  coords (S _ l) = [l]

processInput :: Input -> (WithCoords Number, WithCoords Symbol)
processInput = foldMap (\case Number n -> (withCoords n, emptyWC); Symbol s -> (emptyWC, withCoords s))

type Output = Int

solveA, solveB :: Input -> Output
solveA = processInput >>> (\((ns, _), (_, symbolSet)) -> filter (nextToSymbol symbolSet) ns) >>> map value >>> sum
 where
  nextToSymbol :: Map Coord Symbol -> Number -> Bool
  nextToSymbol symbolSet = coords >>> concatMap neighbors8 >>> any (`M.member` symbolSet)
solveB = processInput >>> (\((_, ns), (syms, _)) -> mapMaybe (isGear ns) syms) >>> map (uncurry (*)) >>> sum
 where
  isGear :: Map Coord Number -> Symbol -> Maybe (Int, Int)
  isGear ns (S '*' loc) = case getAdjacentNumbers loc of
    [n1, n2] -> Just (value n1, value n2)
    _ -> Nothing
   where
    getAdjacentNumbers :: Coord -> [Number]
    getAdjacentNumbers = neighbors8 >>> mapMaybe (ns M.!?) >>> nub
  isGear _ _ = Nothing

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Coord = (Int, Int)

above, below, left, right :: Coord -> Coord
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)

neighbors :: Coord -> [Coord]
neighbors = applyAll [above, below, left, right]

neighbors8 :: Coord -> [Coord]
neighbors8 = applyAll [above, above . left, left, below . left, below, below . right, right, above . right]
