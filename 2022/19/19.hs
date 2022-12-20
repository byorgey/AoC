#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package lens --package generic-lens --package mtl --package hashable

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Arrow         (second, (>>>))
import           Control.Lens
import           Control.Monad         (guard)
import           Control.Monad.State
import           Data.Char             (toUpper)
import           Data.Generics.Product
import           Data.Hashable
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IS
import           Data.List.Split       (splitOn)
import           Data.Map              (Map, (!))
import qualified Data.Map.Strict       as M
import           GHC.Generics

main = interact $
  readInput >>> applyAll [{- solveA, -} solveB] >>> map show >>> unlines

------------------------------------------------------------

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show, Read, Enum, Bounded)
type Blueprint = Map Resource [(Int, Resource)]
type Input = [Blueprint]

allResources :: [Resource]
allResources = [minBound .. maxBound]

readInput :: String -> Input
readInput = lines >>> map readBlueprint

readBlueprint :: String -> Blueprint
readBlueprint = words >>> drop 2 >>> unwords >>> splitOn "." >>> init >>> map (words >>> readCost) >>> M.fromList

readCost :: [String] -> (Resource, [(Int, Resource)])
readCost (_:r:_:_:cs) = (readLower r, readCosts cs)

readCosts :: [String] -> [(Int,Resource)]
readCosts = splitOn ["and"] >>> map (\[n,r] -> (read n, readLower r))

------------------------------------------------------------

data St = St { timeLeft :: !Int, resources :: [Int], robots :: [Int] }
  deriving (Eq, Ord, Show, Generic, Hashable)

initSt t = addRobot Ore $ St t zeros  zeros
  where
    zeros = map (const 0) allResources

harvest :: St -> St
harvest (St t res rob) = St (t-1) (zipWith (+) rob res) rob

onResource :: Resource -> (Int -> Int) -> [Int] -> [Int]
onResource r = onIndex (fromEnum r)
  where
    onIndex _ _ []     = []
    onIndex 0 f (r:rs) = f r:rs
    onIndex i f (r:rs) = r : onIndex (i-1) f rs

addRobot r st = st & field @"robots" %~ onResource r (+1)

useInputs :: [(Int,Resource)] -> St -> Maybe St
useInputs is st = st' <$ guard (all (>= 0) (resources st'))
  where
    st' = foldr (.) id (map useInput is) st
    useInput (k,r) = field @"resources" %~ onResource r (subtract k)

next :: Blueprint -> St -> [St]
next _ st | timeLeft st == 0 = []
next bp st =
    -- Heuristic: first try making the most advanced robot possible,
    -- after which we try not making any robots (to let resources accumulate),
    -- followed by the other robot types
    insert2nd (harvest st)
    [ addRobot rType (harvest st')
    | rType <- reverse allResources
    , Just st' <- [useInputs (bp ! rType) st]
    ]
    where
      insert2nd x []     = [x]
      insert2nd x (y:zs) = y:x:zs

geodes :: St -> Int
geodes (St _ rs _) = rs !! 3

maxGeodes :: St -> Int
maxGeodes (St t rs rbs) = rs !! 3 + sum (take t [rbs !! 3 .. ])

maxObsidian :: Int -> Blueprint -> Int
maxObsidian n bp = best $ branchAndBound (next bp) geodes maxGeodes (initSt n)

type Output = Int

solveA, solveB :: Input -> Output
solveA = zipWith (\i bp -> i * maxObsidian 24 bp) [1 ..] >>> sum
solveB = take 3 >>> map (maxObsidian 32) >>> product

------------------------------------------------------------

data BB a = BB { best :: !Int, seen :: !IntSet, steps :: !Int }
  deriving (Eq, Ord, Show, Generic)

initBB :: BB a
initBB = BB 0 IS.empty 0

branchAndBound :: Hashable a => (a -> [a]) -> (a -> Int) -> (a -> Int) -> a -> BB a
branchAndBound children lowerBound upperBound start = execState (go start) initBB
  where
    go a = do
      BB b s k <- get
      let h = hash a
      when (h `IS.notMember` s && upperBound a > b) $ do
        let m = max (lowerBound a) b
        put (BB m (IS.insert h s) (k+1))
        mapM_ go (children a)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

onHead :: (a -> a) -> [a] -> [a]
onHead _ []     = []
onHead f (a:as) = f a : as

readLower :: Read a => String -> a
readLower = read . onHead toUpper

infixr 0 >$>
(>$>) = flip ($)
