#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package generic-lens --package lens --package mtl

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Arrow         ((>>>))
import           Control.Lens          (at, ix, use, (+=), (.=))
import           Control.Monad.State
import           Data.Generics.Product
import           Data.IntMap           (IntMap, (!))
import qualified Data.IntMap           as IM
import           Data.List             (find)
import           Data.List.Split       (splitOn)
import           Data.Maybe            (fromJust)
import           GHC.Generics          (Generic)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = splitOn "," >>> map read

type Output = Int

runWith n v = initSt >>> evalState (setNounVerb n v >> run >> rd 0)

solveA, solveB :: Input -> Output
solveA = runWith 12 2
solveB m = find' (\(n,v) -> runWith n v m == 19690720) [(n,v) | n <- [0..99], v <- [0..99]] >$> (\(n,v) -> 100 * n + v)

------------------------------------------------------------

data St = St { mem :: IntMap Int, ip :: !Int }
  deriving (Eq, Show, Generic)

initSt :: [Int] -> St
initSt m = St (IM.fromList (zip [0..] m)) 0

wr :: Int -> Int -> State St ()
wr i v = field @"mem" . ix i .= v

rd :: Int -> State St Int
rd i = fromJust <$> use (field @"mem" . at i)

data Instr = Arith Op Int Int Int | Halt deriving (Eq, Show)
data Op = Add | Mul deriving (Eq, Show)

getInstr :: State St Instr
getInstr = do
  p <- use (field @"ip")
  i <- rd p
  case i of
    99 -> return Halt
    _ -> do
      x <- rd (p+1)
      y <- rd (p+2)
      z <- rd (p+3)
      return $ Arith (if i == 1 then Add else Mul) x y z

advanceIP :: State St ()
advanceIP = field @"ip" += 4

-- Bool = whether to keep running
step :: State St Bool
step = do
  i <- getInstr
  case i of
    Halt -> return False
    Arith op x y z -> do
      a <- rd x
      b <- rd y
      wr z (interpOp op a b)
      advanceIP
      return True

interpOp Add = (+)
interpOp Mul = (*)

run :: State St ()
run = do
  cont <- step
  when cont run

setNounVerb n v = wr 1 n >> wr 2 v

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)
