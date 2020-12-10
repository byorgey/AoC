import           Control.Arrow
import           Data.Array
import           Data.Function
import           Data.List
import           Data.Maybe

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = words >>> map read

type Output = Int

solveA, solveB :: Input -> Output
solveA = (0:) >>> sort >>> (zipWith subtract <*> tail) >>> sort >>> group >>>
  map (head &&& length) >>> (\ns -> fromJust (lookup 1 ns) * (1 + fromJust (lookup 3 ns)))


-- Index 0 = outlet, index n = device; rest are sorted
-- Let w[k] = # ways to connect adapters ending with adapter k
-- w[0] = 1
-- w[k] = w[k-1] + w[k-2] [a[k] - a[k-2] <= 3] + w[k-3] [a[k] - a[k-3] <= 3]

solveB as = w n
  where
    device = maximum as + 3
    n = length as + 1
    a = listArray (0,n) (sort $ 0 : device : as)

    w = memoFix (0,n) w'
    w' r 0 = 1
    w' r k = sum (map prev [1..3])
      where
        prev i = if (k >= i && (a!k - a!(k-i) <= 3)) then r (k-i) else 0

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

toTable :: Ix i => (i, i) -> (i -> a) -> Array i a
toTable rng f = array rng (map (id &&& f) (range rng))

memo :: Ix i => (i,i) -> (i -> a) -> (i -> a)
memo rng = (!) . toTable rng

memoFix :: Ix i => (i,i) -> ((i -> a) -> (i -> a)) -> (i -> a)
memoFix rng f = fix (memo rng . f)
