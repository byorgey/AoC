#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Arrow      ((&&&), (***), (>>>))
import           Data.Array.Unboxed
import           Data.Bool          (bool)
import           Data.Function      (on)
import           Data.List          (foldl', foldr, groupBy, sortOn)
import           Data.List.Split    (splitOn)
import           Data.Map           (Map)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

data Input = Input { algorithm :: UArray Int Bool, image :: Map (Int,Int) Bool }

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> mkInput

mkInput [concat -> alg, img] =
  Input
    (listArray (0,511) (map (=='#') alg))
    (img >$> map (map (=='#')) >>> (flip zipWith [0..] $ \r -> flip zipWith [0..] $ \c -> ((r,c),)) >>> concat >>> M.fromList)

type Output = Int

solveA, solveB :: Input -> Output
solveA = solve 2
solveB = solve 50

solve n Input{..} = Grid False image >$> foldr (>>>) id (replicate n (enhance algorithm)) >>> grid >>> pixelCount

------------------------------------------------------------

data Grid = Grid { dflt :: Bool, grid :: Map (Int,Int) Bool }

expandGrid :: Grid -> Grid
expandGrid (Grid dflt m)
  = Grid dflt $ m
    `M.union` M.fromList [((x,y),dflt) | x <- [xmin-1, xmax+1], y <- [ymin-1 .. ymax+1]]
    `M.union` M.fromList [((x,y),dflt) | y <- [ymin-1, ymax+1], x <- [xmin-1 .. xmax+1]]
  where
    minmax = minimum &&& maximum
    ((xmin,xmax),(ymin,ymax)) = m >$> M.keys >>> unzip >>> (minmax *** minmax)

neighborhood :: Grid -> Map (Int,Int) [Bool]
neighborhood (Grid dflt m) = M.mapWithKey neighbors m
  where
    neighbors (x,y) _ =
      map (flip M.lookup m >>> fromMaybe dflt) [(x',y') | x' <- [x-1 .. x+1], y' <- [y-1 .. y+1]]

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\n d -> 2*n + (if d then 1 else 0)) 0

enhance :: UArray Int Bool -> Grid -> Grid
enhance alg g@Grid{..} = Grid
  (alg!(if dflt then 511 else 0))
  (g >$> expandGrid >>> neighborhood >>> M.map (fromBinary >>> (alg!)))

pixelCount :: Map k Bool -> Int
pixelCount = M.elems >>> filter id >>> length

showGrid :: Map (Int,Int) Bool -> String
showGrid = M.assocs >>> sortOn fst >>> groupBy ((==) `on` (fst >>> fst))
  >>> map (map (snd >>> bool ' ' '#')) >>> unlines

printGrid = putStr . showGrid

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

