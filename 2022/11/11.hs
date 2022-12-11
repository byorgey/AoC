#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package lens --package array --package mtl

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow        ((>>>))
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array
import           Data.List            (sortOn)
import           Data.List.Split      (splitOn)
import           Data.Map             (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (mapMaybe)
import           Data.Ord             (Down (..))
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import           Prelude              hiding (round)
import           Text.Read            (readMaybe)

------------------------------------------------------------

type Input = [MonkeyInfo]
data MonkeyInfo = MonkeyInfo
  { initItems   :: [Int]
  , operation   :: Int -> Int
  , test        :: Int
  , trueMonkey  :: Int
  , falseMonkey :: Int
  }

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> map (map words >>> readMonkey)

readMonkey :: [[String]] -> MonkeyInfo
readMonkey [_, is, op, t, a, b] = MonkeyInfo (getNums is) (readOp op) (getNum t) (getNum a) (getNum b)

getNums :: Read a => [String] -> [a]
getNums = mapMaybe readMaybe

getNum :: Read a => [String] -> a
getNum = getNums >>> head

readOp [_,_,_,_,op,x] old = toFun op old (case x of {"old" -> old; n -> read n})
  where
    toFun "+" = (+)
    toFun "*" = (*)

------------------------------------------------------------

data Monkey = Monkey { _items :: Seq Int, _activity :: Int }
makeLenses ''Monkey
data St = St { _round :: Int, _turn :: Int, _monkeys :: Map Int Monkey }
makeLenses ''St

initSt :: Input -> St
initSt ms = St 0 0 (M.fromList (zip [0..] (map (initItems >>> Seq.fromList >>> flip Monkey 0) ms)))

data MonkeyTable = MT
  { _numMonkeys  :: Int
  , _modulus     :: Int
  , _monkeyOp    :: Array Int (Int -> Int)
  , _monkeyTest  :: Array Int Int
  , _monkeyTrue  :: Array Int Int
  , _monkeyFalse :: Array Int Int
  }

makeLenses ''MonkeyTable

mkMT :: Input -> MonkeyTable
mkMT info = MT n (product (map test info)) (mkA operation) (mkA test) (mkA trueMonkey) (mkA falseMonkey)
  where
    n = length info
    mkA f = listArray (0,n-1) (map f info)

type M a = ReaderT MonkeyTable (State St) a

runM :: M a -> Input -> (a, St)
runM m info = runState (runReaderT m (mkMT info)) (initSt info)

execM :: M a -> Input -> St
execM m = snd . runM m

the :: Getter MonkeyTable (Array Int a) -> M a
the g = do
  t <- use turn
  view (g . to (! t))

step :: (Int -> Int) -> M ()
step calm = do
  t <- use turn
  m <- use (monkeys . ix t . items)
  p <- view modulus
  case Seq.viewl m of
    Seq.EmptyL  -> nextMonkey
    i Seq.:< is -> do
      monkeys . ix t . items .= is
      monkeys . ix t . activity += 1
      op <- the monkeyOp
      let i' = calm (op i) `mod` p
      d <- the monkeyTest
      target <- case i' `mod` d == 0 of
        True  -> the monkeyTrue
        False -> the monkeyFalse
      monkeys . ix target . items %= (Seq.|> i')

runRound :: (Int -> Int) -> M ()
runRound calm = do
  r <- use round
  step calm
  r' <- use round
  when (r == r') (runRound calm)

nextMonkey :: M ()
nextMonkey = do
  t <- use turn
  n <- view numMonkeys
  if t == n-1
    then do {turn .= 0; round += 1}
    else turn += 1

monkeyBusiness :: St -> Int
monkeyBusiness (St _ _ ms) = ms >$> M.elems >>> map (view activity) >>> sortOn Down >>> take 2 >>> product

------------------------------------------------------------

main = interact $
  filter (`notElem` ":,") >>> readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Output = Int

solveA, solveB :: Input -> Output
solveA = execM (replicateM 20 (runRound (`div` 3))) >>> monkeyBusiness
solveB = execM (replicateM 10000 (runRound id)) >>> monkeyBusiness

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
