#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative (Applicative (liftA2))
import           Control.Arrow       ((>>>))
import           Data.Map            (Map, (!))
import qualified Data.Map            as M

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

------------------------------------------------------------

data Job = Lit Int | Op String Char String
data Monkey = Monkey { name :: String, job :: Job }
type Input = [Monkey]

readInput :: String -> Input
readInput = lines >>> map (words >>> readMonkey)

readMonkey [init -> nm, k]          = Monkey nm (Lit (read k))
readMonkey [init -> nm, x, [op], y] = Monkey nm (Op x op y)

------------------------------------------------------------

type Output = Int

solveA :: Input -> Output
solveA monkeys = m!"root"
  where
    m = M.fromList (map (\(Monkey nm jb) -> (nm, interp jb)) monkeys)
    interp (Lit n)     = n
    interp (Op x op y) = interpOp op (m!x) (m!y)

interpOp '+' = (+)
interpOp '-' = (-)
interpOp '*' = (*)
interpOp '/' = div

------------------------------------------------------------

data MTree = Leaf (Maybe Int) | Branch (Maybe Int) Char MTree MTree deriving (Eq, Show)

treeVal :: MTree -> Maybe Int
treeVal (Leaf v)         = v
treeVal (Branch v _ _ _) = v

mkTree :: [Monkey] -> MTree
mkTree monkeys = m!"root"
  where
    m = M.fromList (map (\(Monkey nm jb) -> (nm, monkeyTree nm jb)) monkeys)
    monkeyTree "root" (Op x _ y) = Branch Nothing '=' (m!x) (m!y)
    monkeyTree "humn" _ = Leaf Nothing
    monkeyTree nm (Lit n) = Leaf (Just n)
    monkeyTree nm (Op x op y) = Branch (liftA2 (interpOp op) (treeVal l) (treeVal r)) op l r
      where
        l = m!x
        r = m!y

solveTree :: MTree -> Int
solveTree (Branch _ _ l r) = case (treeVal l, treeVal r) of
  (Nothing, Just y) -> go y l
  (Just x, Nothing) -> go x r

  where
    go n (Leaf _) = n
    go n (Branch _ op l r) = case (treeVal l, treeVal r) of
      (Nothing, Just y) -> go (invertOp op y n) l
      (Just x, Nothing) -> go (invertOpL op x n) r

invertOp '+' y = subtract y
invertOp '-' y = (+y)
invertOp '*' y = (`div` y)
invertOp '/' y = (*y)

invertOpL '-' x = (x-)
invertOpL '/' x = (x `div`)
invertOpL '+' x = subtract x
invertOpL '*' x = (`div` x)

solveB :: Input -> Output
solveB = mkTree >>> solveTree

------------------------------------------------------------
