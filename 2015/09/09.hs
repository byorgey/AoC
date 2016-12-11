import           Data.List
import qualified Data.Map  as M

data Distance = D String String Integer
type DistMap = M.Map (String,String) Integer

readDistance :: String -> Distance
readDistance s = case words s of [p1,_,p2,_,d] -> D p1 p2 (read d)

makeDistanceMap :: [Distance] -> DistMap
makeDistanceMap ds = M.fromList (concatMap mkEdges ds)
  where
    mkEdges (D p1 p2 d) = [((p1,p2), d), ((p2,p1), d)]

bestDistance :: ([Integer] -> Integer) -> DistMap -> Integer
bestDistance pick m = pick (map routeLen routes)
  where
    places = nub . map fst $ M.keys m
    routes = permutations places
    routeLen route = sum $ map (m M.!) (zip route (tail route))

shortestDistance = bestDistance minimum
longestDistance  = bestDistance maximum

main = do
  f <- getContents
  let m = makeDistanceMap $ map readDistance (lines f)

  print (shortestDistance m)
  print (longestDistance m)
