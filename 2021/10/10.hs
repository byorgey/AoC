import qualified Numeric as N
import           Control.Arrow
import           Data.Char (ord)
import           Data.List (elemIndex, sort)
import           Data.Maybe (fromJust)

main = interact $
  lines >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

solveA, solveB :: [String] -> Int
solveA = map parseLine >>> (\rs -> [ score x | Mismatched x <- rs ]) >>> sum
solveB = map parseLine >>> (\rs -> [ val5 s | Incomplete s <- rs ]) >>> median

------------------------------------------------------------

type Stack = []

data ParseResult
  = OK
  | Mismatched Char
  | Incomplete (Stack Char)
  | Extra [Char]
  deriving (Show)

isOpen = (`elem` "([{<")
match a b = abs (ord b - ord a) <= 2

parseLine = go []
  where
    go :: Stack Char -> [Char] -> ParseResult
    go [] [] = OK
    go stk [] = Incomplete stk
    go stk (c:cs) | isOpen c = go (c : stk) cs
    go (o:stk) (c:cs)
      | match o c = go stk cs
      | otherwise = Mismatched c
    go [] cs = Extra cs

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

val5 = readBase 5 " ([{<"

readBase :: Int -> [Char] -> String -> Int
readBase b digits s =
  case N.readInt b (`elem` digits) (flip elemIndex digits >>> fromJust) s of
    ((a,_):_) -> a

median = sort >>> middle
  where
    middle xs = xs !! (length xs `div` 2)
