import           Control.Arrow
import           Data.List
import           Data.Maybe

main = interact $ readInput >>> solve >>> show

type Input = [Int]

readInput :: String -> Input
readInput = lines >>> map read

type Output = Int

mm :: Int
mm = 20201227

newtype M = M { unM :: Int } deriving (Eq, Ord)

instance Num M where
  fromInteger x = M (fromInteger x `mod` mm)
  negate (M x)  = M ((-x) `mod` mm)
  M x + M y = M ((x + y) `mod` mm)
  M x * M y = M ((x * y) `mod` mm)
  abs = undefined
  signum = undefined

solve :: Input -> Output
solve [x,y] = unM ((M y) ^ a)
  where
    a = dlg 7 x mm
    -- b = dlg 7 y mm

-- dlg b n m computes the discrete base-b logarithm of n mod m (using
-- a naive linear search).
dlg :: Int -> Int -> Int -> Int
dlg b n m = (iterate ((*b) >>> (`mod` m)) 1) >$>
  zip [0 :: Int ..] >>> find' (snd >>> (==n)) >>> fst

------------------------------------------------------------

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust

infixr 0 >$>
(>$>) = flip ($)
