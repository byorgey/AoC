{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Intcode where

import           Control.Arrow         ((>>>))
import           Control.Lens          (at, ix, use, (+=), (.=))
import           Control.Monad.State
import           Data.Generics.Product
import           Data.IntMap           (IntMap, (!))
import qualified Data.IntMap           as IM
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           GHC.Generics          (Generic)

data Machine = St { mem :: IntMap Int, ip :: !Int }
  deriving (Eq, Show, Generic)

initMachine :: [Int] -> Machine
initMachine m = St (IM.fromList (zip [0..] m)) 0

wr :: Int -> Int -> State Machine ()
wr i v = field @"mem" . ix i .= v

rd :: Int -> State Machine Int
rd i = fromJust <$> use (field @"mem" . at i)

data Instr = Arith Op Int Int Int | Halt deriving (Eq, Show)
data Op = Add | Mul deriving (Eq, Show)

getInstr :: State Machine Instr
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

advanceIP :: State Machine ()
advanceIP = field @"ip" += 4

-- Bool = whether to keep running
step :: State Machine Bool
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

run :: State Machine ()
run = do
  cont <- step
  when cont run

setNounVerb n v = wr 1 n >> wr 2 v
