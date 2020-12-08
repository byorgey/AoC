{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Data.Array
import           Data.Char
import           Data.Either
import           Data.Set            (Set)
import qualified Data.Set            as S

------------------------------------------------------------

type Input = BootCode
type BootCode = [Instr]
data Instr = I Op Int deriving (Show, Eq, Ord)
data Op = Acc | Jmp | Nop deriving (Show, Read, Eq, Ord, Bounded, Enum)

type Output = Int

type Loc = Int
data BootCodeState =
  BCS { _code :: Array Int Instr, _pc :: Loc, _acc :: Int, _seen :: Set Loc }

makeLenses ''BootCodeState

------------------------------------------------------------

readInput :: String -> Input
readInput = lines >>> map (words >>> readBootCode)
  where
    readBootCode [op,arg] = I (read (onHead toUpper op)) (read (filter (/='+') arg))

------------------------------------------------------------

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

initBCS :: BootCode -> BootCodeState
initBCS code = BCS (listArray (0,length code-1) code) 0 0 S.empty

-- returns True if the program halted
step :: State BootCodeState Bool
step = do
  loc <- use pc
  (_,n) <- uses code bounds
  case loc > n of
    True  -> return True
    False -> do
      seen %= S.insert loc
      i <- (!loc) <$> use code
      execInstr i
      return False

execInstr :: Instr -> State BootCodeState ()
execInstr (I Acc n) = (acc += n) >> (pc += 1)
execInstr (I Jmp n) = (pc += n)
execInstr (I Nop _) = (pc += 1)

hitInfiniteLoop :: State BootCodeState Bool
hitInfiniteLoop = S.member <$> use pc <*> use seen

-- Left = hit infinite loop; Right = terminates
run :: State BootCodeState (Either Int Int)
run = do
  b <- hitInfiniteLoop
  case b of
    True  -> Left <$> use acc
    False -> do
      h <- step
      case h of
        True  -> Right <$> use acc
        False -> run

alterations :: BootCode -> [BootCode]
alterations []     = [[]]
alterations (I Acc n : is) = map (I Acc n :) (alterations is)
alterations (I op n : is) =
  (I (other op) n : is) : map ((I op n) :) (alterations is)

other Jmp = Nop
other Nop = Jmp
other _   = error "oops"

solveA, solveB :: Input -> Output
solveA = initBCS >>> evalState run >>> (\(Left n) -> n)
solveB = alterations >>> map (initBCS >>> evalState run) >>>
  filter isRight >>> head >>> (\(Right n) -> n)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

onHead :: (a -> a) -> [a] -> [a]
onHead _ []     = []
onHead f (a:as) = f a : as
