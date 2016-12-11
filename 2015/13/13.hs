import           Data.List
import qualified Data.Map   as M
import           Data.Tuple

data Seating = S String String Integer
type HappinessMap = M.Map (String,String) Integer

readSeating :: String -> Seating
readSeating s =
  case words s of
    [p1,_,sign,n,_,_,_,_,_,_,p2]
      -> S p1 p2 ((if sign=="lose" then negate else id) (read n))

makeHappinessMap :: [Seating] -> HappinessMap
makeHappinessMap ss = M.fromList (map mkEdge ss)
  where
    mkEdge (S p1 p2 n) = ((p1,init p2), n)

bestSeating :: HappinessMap -> Integer
bestSeating m = maximum (map seatingScore seatings)
  where
    people = nub . map fst $ M.keys m
    seatings = permutations people  -- gives duplicates but who cares
    seatingScore s = sum $ map (\pr -> m ! pr + m ! swap pr) (zip s (tail s ++ [head s]))

    mp ! k
      | M.member k mp = mp M.! k
      | otherwise     = 0

main = do
  f <- getContents
  let m = makeHappinessMap $ map readSeating (lines f)

  print (bestSeating m)
  print (bestSeating (M.insert ("me","me") 0 m))

