{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Intcode where

import           Control.Arrow         ((>>>))
import           Control.Lens          (at, imapM, ix, use, (%=), (+=), (.=))
import           Control.Monad.State
import           Data.Generics.Product (field)
import           Data.IntMap           (IntMap, (!))
import qualified Data.IntMap           as IM
import           Data.List             (find)
import           Data.List.Split       (splitOn)
import           Data.Maybe            (fromJust)
import           GHC.Generics          (Generic)

data Machine = M { mem :: IntMap Int, ip :: !Int, nextIP :: !Int, input :: [Int], output :: [Int] }
  deriving (Eq, Show, Generic)

readIntcode :: String -> [Int]
readIntcode = splitOn "," >>> map read

initMachine :: [Int] -> Machine
initMachine = initMachineIO []

initMachineIO :: [Int] -> [Int] -> Machine
initMachineIO ins m = M (IM.fromList (zip [0..] m)) 0 0 ins []

wr :: Int -> Int -> State Machine ()
wr i v = field @"mem" . ix i .= v

rd :: Int -> State Machine Int
rd i = fromJust <$> use (field @"mem" . at i)

data Mode = Pos | Imm
  deriving (Eq, Show)

readMode :: Int -> Mode
readMode = \case
  0 -> Pos
  1 -> Imm

data Param = Param Mode Int
  deriving (Eq, Show)

data Instr = Instr Op [Param]
  deriving (Eq, Show)

data Op = Add | Mul | Input | Output | JIT | JIF | Less | Equal | Halt
  deriving (Eq, Show)

inputs :: Op -> Int
inputs = \case
  Add    -> 2
  Mul    -> 2
  Input  -> 0
  Output -> 1
  JIT    -> 2
  JIF    -> 2
  Less   -> 2
  Equal  -> 2
  Halt   -> 0

arity :: Op -> Int
arity = \case
  Add    -> 3
  Mul    -> 3
  Input  -> 1
  Output -> 1
  JIT    -> 2
  JIF    -> 2
  Less   -> 3
  Equal  -> 3
  Halt   -> 0

readOp :: Int -> Op
readOp = \case
  1  -> Add
  2  -> Mul
  3  -> Input
  4  -> Output
  5  -> JIT
  6  -> JIF
  7  -> Less
  8  -> Equal
  99 -> Halt

decodeInstr :: State Machine Instr
decodeInstr = do
  p <- use (field @"ip")
  i <- rd p
  let opcode = readOp (i `mod` 100)
      modes = map (readMode . (`mod` 10)) . take (arity opcode) . iterate (`div` 10) $ i `div` 100
  ps <- imapM (\j m -> Param m <$> rd (p+j+1)) modes
  field @"nextIP" .= p + arity opcode + 1
  return $ Instr opcode ps

advanceIP :: State Machine ()
advanceIP = do
  n <- use (field @"nextIP")
  field @"ip" .= n

loadParam :: Param -> State Machine Int
loadParam (Param m i) = case m of
  Pos -> rd i
  Imm -> return i

writeAddr :: [Param] -> Int
writeAddr = last >>> (\(Param _ i) -> i)

-- Bool = whether to keep running
step :: State Machine Bool
step = do
  Instr op ps <- decodeInstr
  vs <- mapM loadParam (take (inputs op) ps)
  case op of
    Add -> wr (writeAddr ps) (sum vs)
    Mul -> wr (writeAddr ps) (product vs)
    Input -> do
      is <- use (field @"input")
      field @"input" .= tail is
      wr (writeAddr ps) (head is)
    Output -> field @"output" %= (head vs :)
    JIT -> when (vs!!0 /= 0) (field @"nextIP" .= vs!!1)
    JIF -> when (vs!!0 == 0) (field @"nextIP" .= vs!!1)
    Less -> wr (writeAddr ps) (fromEnum $ vs!!0 < vs!!1)
    Equal -> wr (writeAddr ps) (fromEnum $ vs!!0 == vs!!1)
    Halt -> return ()
  when (op /= Halt) advanceIP
  return (op /= Halt)

run :: State Machine ()
run = do
  cont <- step
  when cont run

setNounVerb n v = wr 1 n >> wr 2 v
