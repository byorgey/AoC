
import           Data.List
import           Data.Monoid
import           Data.Ord
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

threePartitionsOfWeight :: Int -> [Int] -> [[Int]]
threePartitionsOfWeight w xs =
  [ s1
  | s1 <- subsetsOfWeight w xs
  , not . null $ subsetsOfWeight w (xs \\ s1)
  ]

fourPartitionsOfWeight :: Int -> [Int] -> [[Int]]
fourPartitionsOfWeight w xs =
  [ s1
  | s1 <- subsetsOfWeight w xs
  , any (\s2 -> not . null $ subsetsOfWeight w ((xs \\ s1) \\ s2)) $ subsetsOfWeight w (xs \\ s1)
  ]

main = do
  items <- (map read . lines) <$> getContents
  let totalWeight = sum items
  print . product . head . sortBy (comparing length <> comparing product) $ threePartitionsOfWeight (totalWeight `div` 3) items

  print . product . head . sortBy (comparing length <> comparing product) $ fourPartitionsOfWeight (totalWeight `div` 4) items

