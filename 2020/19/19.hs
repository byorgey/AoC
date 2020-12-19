{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow
import           Data.List.Split
import           Data.Map        (Map, (!))
import qualified Data.Map        as M

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

data Input = Input { rules :: Map Int Rule, messages :: [String] }
data Rule = C Char | R [[Int]]  -- sum of products

readInput :: String -> Input
readInput = lines >>>
  span (not.null) >>> ((map readRule >>> M.fromList) *** drop 1) >>> uncurry Input

readRule :: String -> (Int, Rule)
readRule = words >>> process
  where
    process (label : rule) = (read (init label), processRule rule)
    processRule ['"':c:_] = C c
    processRule rs        = R (rs >$> splitOn ["|"] >>> map (map read))

type Output = Int

------------------------------------------------------------

-- Unfold a map of labelled rules into a tree representing the
-- grammar, getting rid of the intermediate labels entirely.  Note, if
-- there are recursive rules, as in part 2, this will be a lazy,
-- infinite tree, but that is no problem for Haskell!  Also note, this
-- could potentially lead to bad combinatorial blowup with certain
-- specific inputs (e.g.  0: 1 1, 1: 2 2, 2: 3 3, ...) but that
-- doesn't happen with the puzzle inputs.
data Grammar
  = Leaf Char      -- base case: a single char
  | Alt [Grammar]  -- a list of alternatives
  | Seq [Grammar]  -- a sequence

-- Recursively build a grammar tree from a rule map, starting with
-- rule 0.
buildGrammar :: Map Int Rule -> Grammar
buildGrammar m = go 0
  where
    go label = case m!label of
      C c  -> Leaf c
      R rs -> rs >$> map (map go >>> Seq) >>> Alt

-- Match a grammar with a string, and return a list of potential
-- remainders after removing a matched portion from the beginning.
match :: Grammar -> String -> [String]
match (Leaf _) [] = []
match (Leaf c) (x:xs)
  | c == x    = [xs]
  | otherwise = []
match (Alt gs) s     = concatMap (`match` s) gs
match (Seq []) s     = [s]
match (Seq (g:gs)) s = match g s >>= match (Seq gs)

-- A string completely matches a grammar if it's possible for the
-- grammar to successfully match returning the empty string as the
-- remainder.
matches :: Grammar -> String -> Bool
matches g s = any null (match g s)

------------------------------------------------------------

solveA, solveB :: Input -> Output
solveA = solveWith id
solveB = solveWith part2Rules

part2Rules = M.insert 8 (R [[42],[42,8]]) >>> M.insert 11 (R [[42,31],[42,11,31]])

solveWith f (Input{..}) = count (matches (buildGrammar (f rules))) messages

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
