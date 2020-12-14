{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import           Data.Char
import           Data.IntMap         (IntMap, (!))
import qualified Data.IntMap         as M
import           Numeric             (readInt)

------------------------------------------------------------

type Input = [Instr]
data Instr = SetMask Mask | Assign Int Int  deriving (Show, Eq)
data Mask = Mask { zeros :: Int, ones :: Int, xs :: Int } deriving (Show, Eq)

data St = St { _mem :: IntMap Int, _mask :: Mask }

makeLenses ''St

initSt = St (M.empty) (Mask 0 0 0)

------------------------------------------------------------

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

readInput :: String -> Input
readInput = lines >>> map (words >>> readInstr)
  where
    readInstr ["mask",_,m] = SetMask (readMask m)
    readInstr [l,_,v]      = Assign (readMem l) (read v)

readMask :: String -> Mask
readMask m = Mask (readBinOnly '0' m) (readBinOnly '1' m) (readBinOnly 'X' m)
  where
    readBinOnly b = map (\x -> if x == b then '1' else '0') >>> readBin

readBin :: String -> Int
readBin = readInt 2 (`elem` "01") (\c -> ord c - ord '0') >>> head >>> fst

readMem :: String -> Int
readMem = dropWhile (/='[') >>> drop 1 >>> init >>> read

type Output = Int

------------------------------------------------------------

exec :: Instr -> State St ()
exec (SetMask m)  = mask .= m
exec (Assign l v) = do
  v' <- useMask v
  mem %= M.insert l v'

useMask :: Int -> State St Int
useMask v = do
  m <- use mask
  return $ applyMask m v

applyMask :: Mask -> Int -> Int
applyMask (Mask z o _) = (.|. o) >>> complement >>> (.|. z) >>> complement

------------------------------------------------------------

exec2 :: Instr -> State St ()
exec2 (SetMask m)  = mask .= m
exec2 (Assign l v) = do
  ls <- maskAddress l
  mapM_ (\l' -> mem %= M.insert l' v) ls

maskAddress :: Int -> State St [Int]
maskAddress l = do
  (Mask{..}) <- use mask
  return $ expand xs (l .|. ones)

-- https://cp-algorithms.com/algebra/all-submasks.html
expand :: Int -> Int -> [Int]
expand m l = map (\m' -> maskWith m m' l) (takeUntil (==0) (iterate (\s -> (s-1) .&. m) m))
  where
    maskWith super sub v = (v .&. complement super) .|. sub

takeUntil p [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

------------------------------------------------------------

solveA, solveB :: Input -> Output
solveA = solveWith exec
solveB = solveWith exec2

solveWith e prog = execState (mapM_ e prog) initSt >$>
  view mem >>> M.elems >>> filter (/= 0) >>> sum

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
