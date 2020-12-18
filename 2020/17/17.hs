import           Control.Arrow
import           Data.Array.Unboxed
import           Data.List
import           GHC.Arr            (Ix (..))

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Bool]]

readInput :: String -> Input
readInput = lines >>> map (map (=='#'))

type Output = Int

solveA, solveB :: Input -> Output
solveA = solve 3
solveB = solve 4

solve dim = mkGrid dim >>> iterate lifeStep >>> (!!6) >>> elems >>> count id

------------------------------------------------------------

-- Use lists as array indices, to generically handle
-- arbitrary-dimensional arrays
instance Ix a => Ix [a] where
  range (ls,us) = sequence (zipWith (curry range) ls us)
  unsafeIndex (ls,us) is =
    foldl' (\n ((l,u),i) -> n * unsafeRangeSize (l,u) + unsafeIndex (l,u) i) 0
      (zip (zip ls us) is)

  inRange (ls, us) is = and (zipWith inRange (zip ls us) is)

type LifeGrid = UArray [Int] Bool

mkGrid :: Int -> Input -> LifeGrid
mkGrid dim g = listArray (1:1:pad, r:c:pad) $ concat g
  where
    r = length g
    c = length (head g)
    pad = replicate (dim-2) 1

-- Arbitrary-dimensional life
lifeStep :: LifeGrid -> LifeGrid
lifeStep a = array newRange $ [ (i, rule i) | i <- range newRange ]
  where
    oldRange = bounds a
    newRange = (map pred lo, map succ hi)

    zero  = map (const 0) (fst oldRange)
    cells = map fst . filter snd $ assocs a
    lo    = foldl1' (zipWith min) cells
    hi    = foldl1' (zipWith max) cells

    rule i
      | n == 3    = True
      | n == 2    = if (inRange (bounds a) i) then a!i else False
      | otherwise = False
      where
        n = count (a!) (neighbors i)

    neighbors i = filter (inRange oldRange) . delete i $ mapM (\x -> [x-1,x,x+1]) i

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs
