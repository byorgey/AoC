import           Data.List
import           Data.List.Split (chunksOf)

type Tri = [Int]

rot :: Tri -> Tri
rot [x,y,z] = [y,z,x]

possible :: Tri -> Bool
possible = all possible' . take 3 . iterate rot
  where
    possible' [x,y,z] = x+y > z

-- Part 1
-- main = interact (show . length . filter id . map (possible . map read . words) . lines)

-- Part 2
main = interact
  ( show . length
  . filter possible
  . chunksOf 3
  . concat
  . transpose
  . map (map read . words)
  . lines
  )
