{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

import           Debug.Trace

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.List.Split
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import qualified Data.Set            as S
import           Data.Tree

data Rule = Rule { ruleFrom :: Element, ruleTo :: Molecule }
  deriving Show

readRule :: String -> Rule
readRule = (Rule <$> (!!0) <*> (parseMolecule . tokenize . (!!2))) . words

type Rules = M.Map Element [Rule]

readInput :: String -> (Rules, String)
readInput
  = first (M.fromListWith (++))
  . (first.map) (\r@(Rule from _) -> (from, [r]))
  . (map readRule *** (!!1))
  . span (not.null)
  . lines

replacements :: String -> Rule -> [String]
replacements "" _ = [""]
replacements str@(x:xs) r@(Rule from to)
  | from `isPrefixOf` str = (showMolecule to ++ drop (length from) str) : map (x:) (replacements xs r)
  | otherwise = map (x:) (replacements xs r)

main = do
  (rs,r) <- readInput <$> getContents
  print . pred . S.size . S.unions . map S.fromList . map (replacements r) . concat $ M.elems rs

  print . moleculeSteps . parseMolecule . tokenize $ r

------------------------------------------------------------
-- Part 2
------------------------------------------------------------

type Element = String

tokenize :: String -> [Element]
tokenize = split (startsWithOneOf ['A'..'Z'])

-- Observation: Rn always comes paired with a closing Ar, and both are
-- terminals, so we can match them to split the string into a tree.

data Molecule
  = Leaf Element
  | RnAr Molecule
  | Cat [Molecule]
  deriving (Eq, Show)

parseMolecule :: [Element] -> Molecule
parseMolecule = fst . pm
  where
    pm :: [Element] -> (Molecule, [Element])
    pm [] = (Cat [], [])
    pm ("Ar":es) = (Cat [], "Ar":es)
    pm ("Rn":es) = case pm es of
      (m, "Ar":es') -> first (RnAr m <>) (pm es')
    pm (e:es) = first (Leaf e <>) (pm es)

-- Observation: every rule turns a single symbol into two (if we count
-- a "parenthesized" Rn..Ar section as one symbol).  Within Rn...Ar,
-- sometimes there are multiple things separated by Y, which is a
-- terminal symbol.  So we don't even have to parse in order to count
-- how many steps it must take to generate something, assuming it
-- parses.

moleculeSteps :: Molecule -> Int
moleculeSteps (Leaf e) = 0
moleculeSteps (RnAr m) = moleculeSteps m
moleculeSteps (Cat ms) = sum (map moleculeSteps ms) + sum (map (\m -> length m - 1) ms')
  where
    ms' = splitOn [Leaf "Y"] ms

------------------------------------------------------------
-- Below are all my ineffective, misguided attempts at parsing,
-- preserved for posterity.
------------------------------------------------------------

moleculeElements :: Molecule -> [Element]
moleculeElements (Leaf e) = [e]
moleculeElements (RnAr m) = "Rn" : moleculeElements m ++ ["Ar"]
moleculeElements (Cat ms) = concatMap moleculeElements ms

showMolecule :: Molecule -> String
showMolecule = concat . moleculeElements

headElement :: Molecule -> Element
headElement = head . moleculeElements

lastElement :: Molecule -> Element
lastElement = last . moleculeElements

instance Semigroup Molecule where
  Cat ms1 <> Cat ms2 = Cat (ms1 ++ ms2)
  m1 <> Cat ms2 = Cat (m1 : ms2)
  Cat ms1 <> m2 = Cat (ms1 ++ [m2])
  m1 <> m2 = Cat [m1,m2]

cat :: [Molecule] -> Molecule
cat [m] = m
cat ms  = Cat ms

beginEnd :: Rules -> M.Map Element (S.Set Element, S.Set Element)
beginEnd rs = fixpt $ iterate beginEndStep (M.map (const (S.empty, S.empty)) rs)
  where
    fixpt :: Eq a => [a] -> a
    fixpt = fst . head . dropWhile (uncurry (/=)) . (zip <*> tail)
    beginEndStep :: M.Map Element (S.Set Element, S.Set Element) -> M.Map Element (S.Set Element, S.Set Element)
    beginEndStep beMap
      = flip M.mapWithKey beMap $ \elt _ ->
          ( S.singleton elt `S.union`
              (S.unions $ map (\(Rule _ to) -> maybe (S.singleton (headElement to)) fst
                                                 (M.lookup (headElement to) beMap))
                              (rs M.! elt))
          , S.singleton elt `S.union`
              (S.unions $ map (\(Rule _ to) -> maybe (S.singleton (lastElement to)) snd
                                                 (M.lookup (lastElement to) beMap))
                              (rs M.! elt))
          )

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _ [] = Nothing
safeMaximumBy c as = Just $ maximumBy c as

synthesize :: Rules -> Molecule -> Maybe (Int, Tree Molecule)
synthesize rs m = syn (Leaf "e") m
  where
    beMap = beginEnd rs
    beginSet (Leaf e) = maybe (S.singleton e) fst (M.lookup e beMap)
    beginSet (RnAr _) = S.singleton "Rn"
    endSet   (Leaf e) = maybe (S.singleton e) snd (M.lookup e beMap)
    endSet   (RnAr _) = S.singleton "Ar"

    pickBest = safeMaximumBy (comparing fst) . catMaybes

    syn :: Molecule -> Molecule -> Maybe (Int, Tree Molecule)
--    syn m1 mtarget | traceShow (m1,mtarget) False = undefined
    syn (Cat [m]) target = syn m target
    syn m (Cat [m2]) = syn m m2
    syn (Leaf e1) (Leaf e2) | e1 == e2  = Just (0, Node (Leaf e1) [])
    syn _    (Leaf _) = Nothing
    syn (RnAr m1) (RnAr m2) = syn m1 m2
    syn (Leaf e) (Cat ms) = -- traceShow (fst <$> bestTree) $
      bestTree
      where
        bestTree = pickBest . map (flip syn (Cat ms)) $ rules'
        rules = map ruleTo $ fromMaybe [] (M.lookup e rs)
        rules' = filter (\to -> headMatches ms (beginSet (Leaf $ headElement to))
                             && tailMatches ms (endSet (Leaf $ lastElement to)))
                 rules
      -- try all rules for e where first set of first elt in RHS
      -- includes leftmost thing in ms, and similarly for right end

    syn (Cat ms1) (Cat ms2)
      = safeMaximumBy (comparing fst)
      . map ((((+1) . sum) *** Node (Cat ms1)) . unzip)
      . catMaybes
--      . traceShowId
      $
      [ zipWithM (\m part -> syn m (cat part)) ms1 p
      | p <- partitions ms1 ms2
      ]

    syn _ _ = Nothing

    partitions :: [Molecule] -> [Molecule] -> [[[Molecule]]]
    partitions [] _ = []
    partitions [_]        ms = [[ms]]
    partitions (m1:m2:es) ms = -- traceShow (e1:e2:es) $ traceShow ms $ traceShowId $
      [ ms1 : ps
      | (ms1,ms2) <- splits (endSet m1) (beginSet m2) ms
      , ps <- partitions (m2:es) ms2
      ]

      -- find potential partitions based on first + last sets for
      -- each pair of adjacent items in es, then try each of them

      -- Will need a function that takes last + first set, and breaks
      -- a list of Elements at all possible locations.  Then iterate
      -- that to generate potential partitions.
      --
      -- Those function(s) will probably need to have special cases
      -- for RnAr.

splits :: S.Set Element -> S.Set Element -> [Molecule] -> [([Molecule], [Molecule])]
splits lSet fSet [] = []
splits lSet fSet (m:ms)
  | tailMatches [m] lSet && headMatches ms fSet = ([m],ms) : rest
  | otherwise = rest
  where
    rest = (map.first) (m:) (splits lSet fSet ms)

headMatches :: [Molecule] -> S.Set Element -> Bool
headMatches [] _ = False
headMatches (RnAr _ : _) s = "Rn" `S.member` s
headMatches (Leaf e : _) s = e `S.member` s

tailMatches :: [Molecule] -> S.Set Element -> Bool
tailMatches [] _ = False
tailMatches ms s = case last ms of
  RnAr _ -> "Ar" `S.member` s
  Leaf e -> e `S.member` s

