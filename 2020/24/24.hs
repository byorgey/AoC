import           Control.Arrow
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Set      (Set)
import qualified Data.Set      as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Dir]]
data Dir = E | SE | SW | W | NW | NE
  deriving (Eq, Ord, Show, Enum, Bounded)

readInput :: String -> Input
readInput = lines >>> map readDirs

readDirs []           = []
readDirs ('e':ds)     = E : readDirs ds
readDirs ('s':'e':ds) = SE : readDirs ds
readDirs ('s':'w':ds) = SW : readDirs ds
readDirs ('w':ds)     = W : readDirs ds
readDirs ('n':'w':ds) = NW : readDirs ds
readDirs ('n':'e':ds) = NE : readDirs ds

type Output = Int

solveA, solveB :: Input -> Output
solveA = blackTiles >>> length
solveB = blackTiles >>> S.fromList >>> iterate step >>> (!!100) >>> S.size

------------------------------------------------------------
-- Trilinear coordinates

type T = [Int]

cw :: T -> T
cw [a,b,c] = [-b,-c,-a]

toT :: Dir -> T
toT = (m M.!)
  where
    m = M.fromList $ zip [E .. NE] (iterate cw [0,1,-1])

addT :: T -> T -> T
addT = zipWith (+)

zeroT :: T
zeroT = [0,0,0]

------------------------------------------------------------

blackTiles :: Input -> [T]
blackTiles = map (map toT >>> foldl' addT zeroT) >>>
  sort >>> group >>> filter (length >>> odd) >>> map head

neighbors :: T -> [T]
neighbors t = map (toT >>> addT t) [E .. NE]

step :: Set T -> Set T
step g = newBlack `S.union` oldBlack
  where
    -- The number of black neighbors of each tile
    nbrs :: [(T, Int)]
    nbrs = S.elems g >$> concatMap neighbors >>> sort >>> group >>> map (head &&& length)

    -- Tiles that become (or stay) black because they have exactly 2 black neighbors
    newBlack = S.fromList (filter (snd >>> (==2)) nbrs >$> map fst)

    -- Tiles that stay black because they were black before and have 1 or 2 black neighbors
    oldBlack = g `S.intersection` S.fromList (filter (snd >>> (`elem` [1,2])) nbrs >$> map fst)

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
