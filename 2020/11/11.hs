import           Control.Arrow
import           Data.Array.Unboxed
import           Data.List
import           Data.Maybe

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = Grid
type Grid = UArray (Int,Int) Char

readInput :: String -> Input
readInput inp = listArray ((1,1),(rows,cols)) (concat lst)
  where
    lst = lines inp
    rows = length lst
    cols = length (head lst)

type Output = Int

solveA, solveB :: Input -> Output
solveA = solveWith adjacent 4
solveB = solveWith visible 5

solveWith :: Vis -> Int -> Grid -> Int
solveWith vis crowd g
  = fixpoint (stepGrid (neighborCache vis g) crowd) g >$> elems >>> count (=='#')

stepGrid :: NeighborCache -> Int -> Grid -> Grid
stepGrid nbrs crowd g = array bds $
  [ ((r,c), stepCell nbrs crowd g (r,c))
  | r <- [1 .. rows], c <- [1 .. cols]
  ]
  where
    bds@(_,(rows, cols)) = bounds g

stepCell :: NeighborCache -> Int -> Grid -> (Int,Int) -> Char
stepCell nbrs crowd g i
  | g!i == '.' = '.'
  | n == 0     = '#'
  | n >= crowd = 'L'
  | otherwise = g!i
  where
    n = occupiedNeighbors nbrs g i

occupiedNeighbors :: NeighborCache -> Grid -> (Int,Int) -> Int
occupiedNeighbors nbrs g = (nbrs!) >>> map (g!) >>> count (=='#')

fixpoint :: (Eq a, Show a) => (a -> a) -> a -> a
fixpoint f a
  | a' == a = a
  | otherwise = fixpoint f a'
  where
    a' = f a

type NeighborCache = Array (Int,Int) [(Int,Int)]

neighborCache :: Vis -> Grid -> NeighborCache
neighborCache vis g = array bds [((r,c), neighborhood vis g (r,c)) | r <- [1..rows], c <- [1..cols]]
  where
    bds@(_,(rows,cols)) = bounds g

type Vis = [(Int,Int)] -> [(Int,Int)]

adjacent, visible :: Vis
adjacent = take 1
visible  = id

neighborhood :: Vis -> Grid -> (Int,Int) -> [(Int,Int)]
neighborhood vis g i = catMaybes $ map (findNeighbor vis g i) directions
  where
    directions = [(dr,dc) | dr <- [-1..1], dc <- [-1..1], dr /= 0 || dc /= 0]

findNeighbor :: Vis -> Grid -> (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
findNeighbor vis g i d = i >$> iterate (add d) >>> drop 1 >>> takeWhile inBounds >>> vis >>> find seat
  where
    (_,(rows,cols)) = bounds g
    add (r,c) (dr,dc) = (r+dr, c+dc)
    inBounds (r,c) = 0 < r && r <= rows && 0 < c && c <= cols
    seat i = g!i /= '.'

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
