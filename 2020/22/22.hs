{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Arrow
import           Control.Lens    (both, over)
import           Data.Char
import qualified Data.Foldable   as F
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Prelude         hiding (round)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = ([Int], [Int])

readInput :: String -> Input
readInput = lines >>> break null >>> over both (drop 1 >>> dropWhile (not . all isDigit) >>> map read)

type Output = Int

------------------------------------------------------------

-- I was originally led astray by the idea of a deck being a queue of
-- cards to try fancy data structures for the decks, like
-- Data.Sequence or a banker's queue, with fast amortized enqueue and
-- dequeue.  However, this turns out to be slower than using plain old
-- lists and O(n) enqueue.  The problem is that in order to check
-- whether we've seen the given decks before, we end up converting the
-- fancy data structures to lists every round anyway (unavoidable
-- since the fancy data structures have the property that multiple
-- concrete structures could represent the same conceptual queue),
-- which kills whatever amortized gains we might have made.

type Deck = [Int]
type Decks = (Deck, Deck)

round :: Decks -> Decks
round (x:xs, y:ys)
  | x > y     = (xs ++ [x,y], ys)
  | otherwise = (xs, ys ++ [y,x])

checkWinner :: Decks -> Maybe [Int]
checkWinner ([], deck2) = Just deck2
checkWinner (deck1, []) = Just deck1
checkWinner _           = Nothing

checksum :: [Int] -> Int
checksum xs = sum (zipWith (*) [n, n-1 ..] xs)
  where
    n = length xs

solveA, solveB :: Input -> Output
solveA = iterate round >>> map checkWinner >>> F.asum >>> fromJust >>> checksum

------------------------------------------------------------

data St = St { deck1 :: Deck, deck2 :: Deck, seen :: Set (Deck, Deck), winner :: Maybe Int }

runGame :: Deck -> Deck -> (Int, Int)
runGame d1 d2 = iterate rround (St d1 d2 S.empty Nothing) >$>
  find (winner >>> isJust) >>> fromJust >>> pickWinner

  where
    pickWinner (St d1 _ _ (Just 1)) = (1, checksum d1)
    pickWinner (St _ d2 _ (Just 2)) = (2, checksum d2)

rround :: St -> St
rround st@(St{..})
  | (deck1, deck2) `S.member` seen = st { winner = Just 1 }
  | null deck1 = st { winner = Just 2 }
  | null deck2 = st { winner = Just 1 }
rround st@(St d1@(x:xs) d2@(y:ys) sn _)
  | x <= length xs && y <= length ys
  = case runGame (take x xs) (take y ys) of
      (1, _) -> xWin
      (2, _) -> yWin
  | x > y     = xWin
  | otherwise = yWin
  where
    nextSt d1' d2' = St d1' d2' (S.insert (d1,d2) sn) Nothing
    xWin = nextSt (xs ++ [x,y]) ys
    yWin = nextSt xs (ys ++ [y,x])

solveB = uncurry runGame >>> snd

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
