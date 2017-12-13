import           Control.Arrow
import           Data.List
import           Data.List.Split

readThing :: String -> [[Int]]
readThing = lines >>> map (splitOn ": " >>> map read)

main = do
  firewall <- readThing <$> getContents

  -- Part 1
  print (severity 0 firewall)

  -- Part 2
  print (find (ok firewall) [0..])   -- takes tens of seconds but who cares

severity _ [] = 0
severity i ([d,r]:fw)
  | i < d  = severity (i+1) ([d,r]:fw)
  | i == d && i `mod` (2*(r-1)) == 0 = d*r + severity (i+1) fw
  | otherwise                        = severity (i+1) fw

-- Have to find a delay k such that for each (d,r) we have
--   ((k+d) `mod` (2*(r-1)) /= 0)

ok fw k = all (\[d,r] -> (k+d) `mod` (2*(r-1)) /= 0) fw
