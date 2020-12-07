import           Control.Arrow
import           Data.List
import           Data.List.Split
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA, solveB] >>> map show >>> unlines

type Input = [[String]]
type Output = Int

readInput = lines >>> splitOn [""]

solveA, solveB :: Input -> Output
solveA = map (concat >>> S.fromList >>> S.size) >>> sum
solveB = map (map S.fromList >>> foldl1' S.intersection >>> S.size) >>> sum

applyAll fs x = map ($x) fs
