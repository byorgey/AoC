{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow
import           Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import           Data.List
import           Data.List.Split
import           Data.Maybe

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = lines >>> head >>> map ((:[]) >>> read)

type Output = String

------------------------------------------------------------

-- Cups represents a linked list, where each cup maps to the number of
-- its successor.
type Cups = IntMap Int

data Game = Game { cups :: !Cups, curCup :: !Int }

step' :: Int -> Game -> Game
step' n (Game{..}) = Game cups' next
  where
    (_:removed) = take 4 (iterate (cups!) curCup)
    next        = cups!(last removed)

    dest = find' (`notElem` removed) (drop 1 $ iterate dec curCup)
    dec 1 = n
    dec x = pred x

    afterDest = cups!dest

    cups' = foldr (>>>) id
      [ M.insert curCup next
      , M.insert dest (head removed)
      , M.insert (last removed) afterDest
      ]
      cups

mkGame :: Int -> Input -> Game
mkGame n ccs@(c:cs) = Game cups c
  where
    cupList = ccs ++ [length ccs + 1 .. n] ++ [c]
    cups = M.fromList (zip cupList (tail cupList))

runGame n = mkGame n >>> iterate (step' n)

------------------------------------------------------------

solveA :: Input -> Output
solveA inp = inp >$> runGame (length inp) >>> (!!100) >>> sequenceAfter 1 >>> concatMap show

sequenceAfter :: Int -> Game -> [Int]
sequenceAfter x (Game{..}) = x >$> iterate (cups!) >>> drop 1 >>> takeWhile (/= x)

------------------------------------------------------------

-- Takes ~60s on my laptop.
solveB :: Input -> Output
solveB = runGame 1000000 >>> (!!10000000) >>> nextCups >>> show
  where
    nextCups (Game{..}) = next * cups!next
      where
        next = cups!1

------------------------------------------------------------

infixr 0 >$>
(>$>) = flip ($)

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

------------------------------------------------------------

-- Initial naive code below; worked great for part 1 but definitely
-- not fast enough for part 2.

-- step :: Int -> [Int] -> [Int]
-- step n cups@(cur:(splitAt 3 -> (removed, rest))) = insertAfter dest removed rest ++ [cur]
--   where
--     dest = find' (`notElem` removed) (drop 1 $ iterate dec cur)
--     dec 1 = n
--     dec x = pred x

--     insertAfter x toInsert (a:as)
--       | x == a    = a : toInsert ++ as
--       | otherwise = a : insertAfter x toInsert as

-- runGame cups = iterate (step (length cups)) cups

-- rotateAfter x (a:as)
--   | a == x = as
--   | otherwise = rotateAfter x (as ++ [a])
