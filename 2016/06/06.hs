import           Control.Arrow
import           Data.List
import           Data.Ord

main = interact
  ( (++"\n")
  . map pickCommon
  . transpose
  . lines
  )

-- For part 1, use maximumBy
pickCommon = fst . minimumBy (comparing snd) . map (head &&& length) . group . sort
