#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Control.Arrow   ((>>>))
import           Data.Char       (isLower)
import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

type Input = Map String (Set String)

readInput :: String -> Input
readInput = lines >>> map (splitOn "-") >>>
  concatMap (\xs -> [xs, reverse xs]) >>> foldl' addEdge M.empty
  where
    addEdge g [x,y] = M.insertWith S.union x (S.singleton y) g

type Output = Int

solveA, solveB :: Input -> Output
solveA g = dfs 1 g "start" (M.singleton "start" 1)
solveB g = dfs 2 g "start" (M.singleton "start" 1)

------------------------------------------------------------

dfs :: Int -> Map String (Set String) -> String -> Map String Int -> Int
dfs _ _ "end" _ = 1
dfs allowed g u vis = sum $ map try (S.toList (g!u))
  where
    try v
      | v == "start" && visits >= 1 = 0
      | isSmall && visits >= allowed = 0
      | otherwise = dfs allowed' g v (M.insertWith (+) v 1 vis)
      where
        isSmall = all isLower v
        visits = fromMaybe 0 (M.lookup v vis)
        allowed' = min allowed (if isSmall && visits == 1 then 1 else 2)
