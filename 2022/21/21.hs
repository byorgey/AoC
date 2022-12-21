#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns  #-}

import           Control.Applicative (Applicative (liftA2))
import           Control.Arrow       ((>>>))
import           Data.Map            (Map, (!))
import qualified Data.Map            as M

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

------------------------------------------------------------

data Job a = Lit Int | Op a Char a deriving (Eq, Show, Functor)
data Monkey = Monkey String (Job String) deriving (Eq, Show)
type Input = [Monkey]

readInput :: String -> Input
readInput = lines >>> map (words >>> readMonkey)

readMonkey [init -> name, k]          = Monkey name (Lit (read k))
readMonkey [init -> name, x, [op], y] = Monkey name (Op x op y)

interpMonkeyTree :: (String -> Job a -> a) -> [Monkey] -> a
interpMonkeyTree interp monkeys = m!"root"
  where
    m = M.fromList (map (\(Monkey name job) -> (name, interp name (fmap (m!) job))) monkeys)

------------------------------------------------------------

type Output = Int

solveA :: Input -> Output
solveA = interpMonkeyTree interp
  where
    interp _ (Lit n)     = n
    interp _ (Op x op y) = interpOp op x y

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
mkTree = interpMonkeyTree mkNode
  where
    mkNode "root" (Op x _ y) = Branch Nothing '=' x y
    mkNode "humn" _ = Leaf Nothing
    mkNode nm (Lit n) = Leaf (Just n)
    mkNode nm (Op l op r) = Branch (liftA2 (interpOp op) (treeVal l) (treeVal r)) op l r

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
