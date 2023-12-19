#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package megaparsec --package lens

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

import Control.Arrow (first, (&&&), (>>>))
import Control.Lens (both, over)
import Control.Monad (guard)
import Data.Char (toUpper)
import Data.Foldable (asum)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import Data.Map.Strict qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

main =
  interact $
    readInput
      >>> applyAll [solveA, solveB]
      >>> map show
      >>> unlines

------------------------------------------------------------
-- Input: data types + parsing

data Category = X | M | A | S deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Op = Lt | Gt deriving (Eq, Show)
data Condition = Condition Category Op Int deriving (Eq, Show)
data Dest = Accept | Reject | Other String deriving (Eq, Show)
data Rule = Rule {condition :: Condition, dest :: Dest} deriving (Eq, Show)
data Rules = Rules {conditionals :: [Rule], fallback :: Dest} deriving (Eq, Show)
data Workflow = Workflow {workflowName :: String, workflowRules :: Rules} deriving (Eq, Show)
type PartOf a = Map Category a
data Input = Input [Workflow] [Part] deriving (Eq, Show)

type Part = PartOf Int
type PartSet = PartOf Interval

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> \[ws, ps] -> Input (map readWorkflow ws) (map readPart ps)

readWorkflow = readParser parseWorkflow
readPart = readParser parsePart
readParser p = parse p "" >>> either undefined id

type Parser = Parsec Void String

comma = char ','

braces :: Parser a -> Parser a
braces p = char '{' *> p <* char '}'

parseWorkflow :: Parser Workflow
parseWorkflow = Workflow <$> many letterChar <*> braces parseRules

parseRules :: Parser Rules
parseRules = Rules <$> (try parseRule `sepEndBy` comma) <*> parseDest
parseRule = Rule <$> parseCondition <*> (char ':' *> parseDest)
parseDest = Accept <$ char 'A' <|> Reject <$ char 'R' <|> Other <$> many letterChar
parseCondition = Condition <$> parseCategory <*> parseOp <*> decimal
parseOp = Lt <$ char '<' <|> Gt <$ char '>'
parsePart :: Parser Part
parsePart = M.fromList <$> braces (((,) <$> parseCategory <*> (char '=' *> decimal)) `sepBy` comma)
parseCategory = read . (: []) . toUpper <$> oneOf "xmas"

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA (Input ws ps) = ps >$> filter (processPart (workflowMap ws)) >>> map partSum >>> sum
solveB (Input ws _) = universalPartSet >$> processPartSet (workflowMap ws) >>> map partSetSize >>> sum

------------------------------------------------------------

workflowMap :: [Workflow] -> Map String Rules
workflowMap = map (workflowName &&& workflowRules) >>> M.fromList

partSum :: Part -> Int
partSum = M.elems >>> sum

singletonPartSet :: Part -> PartSet
singletonPartSet = fmap singletonI

universalPartSet :: PartSet
universalPartSet = M.fromList [(cat, I 1 4000) | cat <- [minBound .. maxBound]]

partSetSize :: PartSet -> Int
partSetSize = M.elems >>> map sizeI >>> product

processPart :: Map String Rules -> Part -> Bool
processPart m = singletonPartSet >>> processPartSet m >>> null >>> not

-- Return a disjoint list of all subsets of the given part set that
-- will be accepted.
processPartSet :: Map String Rules -> PartSet -> [PartSet]
processPartSet m = applyWorkflow (m ! "in")
 where
  applyWorkflow (Rules rs fb) = applyRules rs fb

  applyRules [] fb ps = sendTo fb ps
  applyRules (r : rs) fb ps = maybe [] (uncurry sendTo) yes ++ maybe [] (applyRules rs fb) no
   where
    (yes, no) = applyRule r ps

  sendTo dest ps = case dest of
    Accept -> [ps]
    Reject -> []
    Other wf -> applyWorkflow (m ! wf) ps

applyRule :: Rule -> PartSet -> (Maybe (Dest, PartSet), Maybe PartSet)
applyRule (Rule cond dest) ps = (first . fmap) (dest,) (applyCond cond ps)

applyCond :: Condition -> PartSet -> (Maybe PartSet, Maybe PartSet)
applyCond (Condition cat op n) ps = over (both . traverse) (replaceCat cat ps) (applyCondI op n (ps ! cat))

replaceCat :: Category -> PartOf a -> a -> PartOf a
replaceCat cat p a = M.insert cat a p

applyCondI :: Op -> Int -> Interval -> (Maybe Interval, Maybe Interval)
applyCondI op n i = over both (nonEmpty . (∩ i)) opIs
 where
  opIs = case op of
    Lt -> (I 1 (n - 1), I n 4000)
    Gt -> (I (n + 1) 4000, I 1 n)

------------------------------------------------------------
-- Utilities

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

data Interval = I {lo :: Int, hi :: Int} deriving (Eq, Ord, Show)

singletonI :: Int -> Interval
singletonI i = I i i

(∪), (∩) :: Interval -> Interval -> Interval
I l1 h1 ∪ I l2 h2 = I (min l1 l2) (max h1 h2)
I l1 h1 ∩ I l2 h2 = I (max l1 l2) (min h1 h2)

isEmpty :: Interval -> Bool
isEmpty (I l h) = l > h

nonEmpty :: Interval -> Maybe Interval
nonEmpty i = i <$ guard (not (isEmpty i))

sizeI :: Interval -> Int
sizeI (I l h) = h - l + 1
