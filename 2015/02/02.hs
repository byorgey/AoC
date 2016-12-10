import           Control.Arrow
import           Data.List
import           Data.List.Split

main = interact
  ( show
  . (sum *** sum)
  . unzip
  . map ((neededPaper &&& neededRibbon) . sort . map read . splitOn "x")
  . lines
  )
  where
    neededPaper [x,y,z] = 3*x*y + 2*x*z + 2*y*z
    neededRibbon [x,y,z] = 2*x + 2*y + x*y*z
