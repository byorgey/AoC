#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Algorithm.Search (dijkstra)
import           Control.Arrow    ((>>>))
import           Data.Char        (isAlpha, ord)
import           Data.List        (find, transpose, (\\))
import           Data.Map         (Map, (!))
import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Char]]

readInput :: String -> Input
readInput = lines >>> drop 2 >>> take 2 >>> map (filter isAlpha) >>> transpose

part2 :: Input -> Input
part2 = zipWith interject p2
  where
    p2 = ["DD","CB","BA","AC"]

    interject xs [a,b] = a : xs ++ [b]

type Output = Int

solveA, solveB :: Input -> Output
solveA = initBurrow >>> dijkstra (next 2) (cost 2) goal >>> fromJust >>> fst
solveB = part2 >>> initBurrow >>> dijkstra (next 4) (cost 4) goal >>> fromJust >>> fst

------------------------------------------------------------

hallwayIxs, roomIxs :: [Int]
roomIxs = [2,4,6,8]
hallwayIxs = [0..10] \\ roomIxs

home :: Char -> Int
home a = 2*(ord a - ord 'A' + 1)

newtype Burrow = Burrow { getBurrow :: Map Int [Char] }
  deriving (Eq, Ord, Show)

initBurrow :: Input -> Burrow
initBurrow = Burrow . M.fromList . (++ zip hallwayIxs (repeat [])) . zip roomIxs

goal :: Burrow -> Bool
goal (Burrow b) = all (null . (b!)) hallwayIxs && all (\a -> all (==a) (b!home a)) "ABCD"

-- For testing depth-2 burrows only
showBurrow :: Burrow -> String
showBurrow (Burrow b) = unlines
  [ replicate 13 '#'
  , '#' : map showHallway [0 .. 10] ++ "#"
  , "###" ++ map showRoomTop [2 .. 8] ++ "###"
  , "  #" ++ map showRoomBot [2 .. 8] ++ "#"
  , "  " ++ replicate 9 '#'
  ]
  where
    showHallway i
      | i `elem` hallwayIxs, [a] <- b!i = a
      | otherwise                       = '.'
    showRoomTop i
      | i `elem` roomIxs = case b!i of
          [a,_] -> a
          _     -> '.'
      | otherwise = '#'
    showRoomBot i
      | i `elem` roomIxs = case b!i of
          [a]   -> a
          [_,a] -> a
          _     -> '.'
      | otherwise = '#'

printBurrow :: Burrow -> IO ()
printBurrow = putStr . showBurrow

next :: Int -> Burrow -> [Burrow]
next depth (Burrow b) = concatMap goHome hallwayIxs ++ concatMap moveOut roomIxs
  where
    clearBetween x y = all (null . (b!)) (filter (\i -> (x < i && i < y) || (y < i && i < x)) hallwayIxs)
    goHome i = case b!i of
      [] -> []
      [a] -> if all (==a) (b!(home a)) && length (b!(home a)) < depth && clearBetween i (home a)
        then [Burrow (M.insert i [] . M.adjust (a:) (home a) $ b)]
        else []
    moveOut i = case b!i of
      []       -> []
      as@(a:_) -> if all (==a) as && i == home a then [] else concatMap (move a i) hallwayIxs
    move a i j = if null (b!j) && clearBetween i j
      then [Burrow (M.insert j [a] . M.adjust tail i $ b)]
      else []

cost :: Int -> Burrow -> Burrow -> Int
cost depth (Burrow b1) (Burrow b2) = (abs (r - h) + d) * 10^(ord a - ord 'A')
  where
    diff i = b1!i /= b2!i
    h = find' diff hallwayIxs
    r = find' diff roomIxs

    a = head (b1!h ++ b2!h)

    d = depth - min (length (b1!r)) (length (b2!r))

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust
