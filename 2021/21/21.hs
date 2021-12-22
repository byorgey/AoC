#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow       ((&&&), (>>>))
import           Control.Lens
import           Control.Monad       (replicateM, unless)
import           Control.Monad.State (State, execState)
import           Data.Array
import           Data.Function       (fix)
import           Data.Tuple          (swap)

------------------------------------------------------------

data Player = Player { _pos :: !Int, _points :: !Int }

data Game = Game { _players :: (Player,Player), _die :: [Int], _rolls :: Int }

makeLenses ''Player
makeLenses ''Game

initGame :: [Int] -> Game
initGame [p1,p2] = Game (Player p1 0, Player p2 0) (cycle [1..100]) 0

roll :: State Game Int
roll = do
  r <- uses die head
  die %= tail
  rolls += 1
  return r

won :: State Game Bool
won = do
  p <- use (players . _1 . points)
  return (p >= 1000)

mod1 :: Int -> Int -> Int
mod1 a m = ((a-1) `mod` m) + 1

turn :: State Game ()
turn = do
  r <- sum <$> replicateM 3 roll
  p' <- players . _1 . pos <%= ((+r) >>> (`mod1` 10))
  players . _1 . points += p'

  w <- won
  unless w $ players %= swap

play :: Game -> Game
play = execState go
  where
    go = do
      w <- won
      unless w $ turn >> go

------------------------------------------------------------

-- Let U(p1,s1,p2,s2) = # of universes in which player 1 wins when
-- starting from position p1 with score s1, with player 2 at p2 with
-- score s2.  Likewise T(p1,s1,p2,s2) is total number of universes that result.
--
-- T = 1 when either score is >= 21, sum of resulting Ts otherwise
--
-- U(p1,>=21,_,_) = 1
-- U(_,_,p2,>=21) = 0
-- U(p1,s1,p2,s2) = sum of (T(p2,s2,p1',s1') - U(p2,s2,p1',s1'))
--   for each of the 27 possible dice rolls.

u :: (Int,Int,Int,Int) -> Int
u = memoFix ((1,0,1,0),(10,21,10,21)) u'
  where
    u' _ (_,21,_,_) = 1
    u' _ (_,_,_,21) = 0
    u' r (p1,s1,p2,s2) =
      sum [ t (p2,s2,p1',s1') - r (p2,s2,p1',s1')
          | roll <- map sum (replicateM 3 [1,2,3])
          , let p1' = (p1 + roll) `mod1` 10
          , let s1' = min 21 (s1 + p1')
          ]

t :: (Int,Int,Int,Int) -> Int
t = memoFix ((1,0,1,0),(10,21,10,21)) t'
  where
    t' _ (_,21,_,_) = 1
    t' _ (_,_,_,21) = 1
    t' r (p1,s1,p2,s2) =
      sum [ t (p2,s2,p1',s1')
          | roll <- map sum (replicateM 3 [1,2,3])
          , let p1' = (p1 + roll) `mod1` 10
          , let s1' = min 21 (s1 + p1')
          ]

------------------------------------------------------------

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

readInput :: String -> [Int]
readInput = lines >>> map (words >>> last >>> read)

type Output = Int

solveA, solveB :: [Int] -> Output
solveA = initGame >>> play >>> (\g -> (g ^. rolls) * (g ^. players . _2 . points))
solveB [p1,p2] = max (u s) (t s - u s)
  where
    s = (p1,0,p2,0)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

------------------------------------------------------------
-- DP

toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
toTable rng f = array rng (map (id &&& f) (range rng))

memo :: Ix i => (i,i) -> (i -> a) -> (i -> a)
memo rng = (!) . toTable rng

memoFix :: Ix i => (i,i) -> ((i -> a) -> (i -> a)) -> (i -> a)
memoFix rng f = fix (memo rng . f)
