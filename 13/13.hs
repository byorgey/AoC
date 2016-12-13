import           Data.Bits
import qualified Data.Set  as S

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

isWall x y = odd $ popCount (x*x + 3*x + 2*x*y + y + y*y + 1364)

type Loc = (Int,Int)

next :: Loc -> S.Set Loc
next (x,y) = S.fromList $ filter valid [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  where
    valid (x,y) = x >= 0 && y >= 0 && not (isWall x y)

main = do
  let theBFS = bfs (==(31,39)) next (S.singleton (1,1))
  print . pred . length $ theBFS

  print . S.size . S.unions . take 51 $ theBFS
