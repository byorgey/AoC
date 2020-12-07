import           Control.Arrow
import           Data.List
import           Data.List.Split
import qualified Data.Set        as S

import           Data.Map        (Map, (!))
import qualified Data.Map        as M

import           Debug.Trace

main = interact $
  readInput >>> applyAll [solveA, solveB] >>> map show >>> unlines

type Input = [Rule]
data Rule = Rule { outer :: Bag, inner :: [(Bag, Int)] } deriving Show
type Bag = (String,String)

type Output = Int

infixr 0 >$>
(>$>) = flip ($)

readInput = lines >>> map (words >>> readRule)
  where
    readRule (o1:o2:_:_:is) = Rule (o1,o2) (readInner is)
    readInner ("no":_) = []
    readInner is       = is >$> chunksOf 4 >>> map (init >>> readBag)
    readBag (n:b1:b2:_) = ((b1,b2),read n)

solveA, solveB :: Input -> Output
solveA = goldMap >>> M.assocs >>> count snd >>> pred
solveB = countMap >>> (!("shiny","gold")) >>> pred

-- Lazy maps for the win!

goldMap :: [Rule] -> Map Bag Bool
goldMap rules = g
  where
    g = M.fromList (map canHoldGold rules)
    canHoldGold (Rule b@("shiny","gold") _) = (b,True)
    canHoldGold (Rule b is)                 = (b, any (g!) (map fst is))

countMap :: [Rule] -> Map Bag Int
countMap rules = c
  where
    c = M.fromList (map countContents rules)
    countContents (Rule b is) = (b, 1 + sum (map (\(i,n) -> n*c!i) is))

count p = filter p >>> length
applyAll fs x = map ($x) fs
