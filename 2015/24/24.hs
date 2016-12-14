import           Math.Combinat.Partitions.Set
import           System.Environment

threePartitions :: [a] -> [[[a]]]
threePartitions as = map mkPartition sps
  where
    sps = setPartitionsWithKParts 3 (length as)
    mkPartition (SetPartition ints) = (map . map) ((as!!) . pred) ints

-- Much too slow to just enumerate all 3-partitions of the 29 items.
-- There are already over 7 million 3-partitions of a set of 16 items.
-- I suppose we need some kind of pruning?
--
-- We could start by just enumerating subsets that add up to exactly 1/3
-- the total weight.  There are 536 million subsets of 29 items, but
-- many of them do not need to be considered.
--
-- Yes, that's quite reasonable: there are 429102 such subsets and
-- they are found in about 1.4 seconds.

subsetsOfWeight :: Int -> [Int] -> [[Int]]
subsetsOfWeight 0 _      = [[]]
subsetsOfWeight _ []     = []
subsetsOfWeight w (x:xs)
  | x > w     = subsetsOfWeight w xs
  | otherwise = map (x:) (subsetsOfWeight (w-x) xs) ++ subsetsOfWeight w xs

-- We need to find combinations of subsets that are actually
-- partitions.  Again, probably unreasonable to do this by directly
-- considering subsets.  We need to adapt the subset generation to
-- actually generate partitions.

threePartitionsOfWeight :: Int -> [Int] -> [[[Int]]]
threePartitionsOfWeight w = go w [] w [] w []
  where
    go 0 xs 0 ys 0 zs _ = [[xs,ys,zs]]
    go _ _  _ _  _ _ []  = []
    go wx xs wy ys wz zs (a:as) = undefined

main = do
  items <- (map read . lines) <$> getContents
  let totalWeight = sum items
  print . length $ subsetsOfWeight (totalWeight `div` 3) items

