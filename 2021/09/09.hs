import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed
import           Data.List          (foldl1', sort, transpose)
import           Data.Maybe         (catMaybes, isJust)
import           Data.Set           (Set)
import qualified Data.Set           as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Int]]

readInput :: String -> Input
readInput = lines >>> map (map ((:[]) >>> read))

type Output = Int

solveA, solveB :: Input -> Output
solveA = lowPoints >>> concat >>> catMaybes >>> map succ >>> sum
solveB grid =
  grid >$> lowPointCoords >>> map (basinSize gridArray) >>>
    sort >>> reverse >>> take 3 >>> product
  where
    gridArray = gridToArray grid

------------------------------------------------------------

shiftRight = map (10:)
shiftLeft  = map reverse >>> shiftRight >>> map (reverse >>> tail)
shiftDown  = transpose >>> shiftRight >>> transpose
shiftUp    = reverse >>> shiftDown >>> reverse >>> tail

lowPoints grid = zipWith (zipWith lowPoint) grid minNbr
  where
    minNbr = foldl1' (zipWith (zipWith min)) (applyAll [shiftRight, shiftLeft, shiftDown, shiftUp] grid)

lowPoint x y
  | x < y     = Just x
  | otherwise = Nothing

gridToArray :: [[Int]] -> UArray (Int,Int) Int
gridToArray grid = listArray ((1,1),(rows,cols)) (concat grid)
  where
    rows = length grid
    cols = length (head grid)

lowPointCoords :: [[Int]] -> [(Int,Int)]
lowPointCoords grid =
  grid >$> lowPoints >>> concat >>> zip [(r,c) | r <- [1 .. rows], c <- [1 .. cols]]
  >>> filter (snd >>> isJust) >>> map fst
  where
    rows = length grid
    cols = length (head grid)

basinSize :: UArray (Int, Int) Int -> (Int, Int) -> Int
basinSize grid lowPt = bfs (const False) next (S.singleton lowPt) >$> map S.size >>> sum
  where
    next u = u >$> nbrs >>> filter (inRange (bounds grid)) >>>
      filter (\v -> grid!v < 9 && grid!v > grid!u) >>> S.fromList

    nbrs (r,c) = [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]

------------------------------------------------------------

bfs :: Ord a => (a -> Bool) -> (a -> Set a) -> Set a -> [Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | S.null layer     = []
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
