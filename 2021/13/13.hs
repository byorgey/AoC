#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow   ((>>>))
import           Data.Char       (toUpper)
import           Data.List       (foldl', transpose)
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> unlines

type P2 = [Int]
data Axis = X | Y deriving (Eq, Ord, Show, Read)
data Fold = Fold Axis Int deriving (Eq, Ord, Show)
data Input = Input { points :: [P2], folds :: [Fold] }
  deriving (Eq, Show)

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> (\[ps,fs] -> Input (readPoints ps) (readFolds fs))
  where
    readPoints = map (splitOn "," >>> map read)
    readFolds = map (words >>> last >>> splitOn "=" >>> (\[a,n] -> Fold (read (map toUpper a)) (read n)))

type Output = String

solveA, solveB :: Input -> Output
solveA Input{..} = points >$> map (applyFold (head folds)) >>> S.fromList >>> S.size >>> show
solveB Input{..} = points >$> S.fromList >>> (\ps -> foldl' (flip (S.map . applyFold)) ps folds) >>> displayPoints

------------------------------------------------------------

applyFold :: Fold -> P2 -> P2
applyFold (Fold X m) [x,y]
  | x < m = [x,y]
  | otherwise = [x - 2*(x-m), y]
applyFold (Fold Y m) [x,y]
  | y < m = [x,y]
  | otherwise = [x, y - 2*(y-m)]

displayPoints :: Set P2 -> String
displayPoints ps = unlines
  [ [ if [x,y] `S.member` ps then '#' else ' ' | x <- [0 .. xmax] ] | y <- [0 .. ymax] ]
  where
    [xmax, ymax] = map maximum (transpose (S.toList ps))

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
