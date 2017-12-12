
import Data.List
import Data.List.Split

step "n"  = [1,0,-1]
step "s"  = [-1,0,1]
step "ne" = [1,-1,0]
step "se" = [0,-1,1]
step "sw" = [-1,1,0]
step "nw" = [0,1,-1]

main = do
  steps <- (splitOn "," . init) <$> getContents
  let end = foldl' (zipWith (+)) [0,0,0] $ map step steps
  print (dist end)

  let locs = scanl (zipWith (+)) [0,0,0] $ map step steps
  print (maximum (map dist locs))

dist loc = (sum . map abs $ loc) `div` 2
