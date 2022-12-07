#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package rosezipper --package mtl

import           Control.Arrow       ((>>>))
import           Control.Monad.State
import           Data.List           (isPrefixOf, sort)
import           Data.List.Split
import           Data.Maybe          (fromJust)
import           Data.Tree
import           Data.Tree.Zipper

main = interact $
  readInput >>> buildFS >>> applyAll [solveA,solveB] >>> map show >>> unlines

-- Input ---------------------------------------------------

type Input = [Command]
data Command = CDTop | CDOut | CDIn String | LS [Entry] deriving (Eq, Ord, Show)
type Size = Integer
data Entry = Dir String | File Size String deriving (Eq, Ord, Show)

isDir :: Entry -> Bool
isDir (Dir{}) = True
isDir _       = False

-- Quick & dirty command & output parsing with Data.List.Split and words.

readInput :: String -> Input
readInput = lines >>> split (dropInitBlank . keepDelimsL $ whenElt ("$" `isPrefixOf`)) >>> map readCommand

readCommand :: [String] -> Command
readCommand (c:resp) = case drop 1 (words c) of
  ["cd", "/"]  -> CDTop
  ["cd", ".."] -> CDOut
  ["cd", d]    -> CDIn d
  ["ls"]       -> LS (map readEntry resp)

readEntry :: String -> Entry
readEntry s = case words s of
  ["dir",d] -> Dir d
  [n, f]    -> File (read n) f

type Output = Size

-- Building the filesystem tree ----------------------------

-- Simulate commands while keeping track of position in filesystem
-- using a tree zipper.

leaf :: e -> Tree e
leaf e = Node e []

emptyFS = leaf (Dir "/")

buildTree :: [Command] -> Tree Entry
buildTree = mapM_ exec >>> flip execState (fromTree emptyFS) >>> root >>> toTree

exec :: Command -> State (TreePos Full Entry) ()
exec CDTop    = modify root
exec CDOut    = modify (parent >>> fromJust)
exec (CDIn d) = modify (children >>> findTree (==Dir d) >>> fromJust)
exec (LS es)  = do
  r <- get
  -- If we've already done an ls command here then do nothing.  Not sure if
  -- this is necessary.
  case hasChildren r of
    True  -> pure ()
    False -> modify (modifyTree (\t -> t { subForest = map leaf es }))

findTree :: (a -> Bool) -> TreePos Empty a -> Maybe (TreePos Full a)
findTree p z = do
  t <- nextTree z
  case p (label t) of
    False -> findTree p (nextSpace t)
    True  -> Just t

annotateTotals :: Tree Entry -> Tree (Size, Entry)
annotateTotals = foldTree calcSize
  where
    calcSize :: Entry -> [Tree (Size, Entry)] -> Tree (Size, Entry)
    calcSize e@(File s _) _  = leaf (s, e)
    calcSize e@(Dir {})   cs = Node (sum (map (fst.rootLabel) cs), e) cs

buildFS = buildTree >>> annotateTotals

-- Solving ----------------------------------------------------------

dirSizes = flatten >>> filter (isDir.snd) >>> map fst

solveA, solveB :: Tree (Size, Entry) -> Output
solveA = dirSizes >>> filter (<= 10^5) >>> sum
solveB t = t >$> dirSizes >>> sort >>> dropWhile (< req) >>> head
  where
    req = fst (rootLabel t) - 40000000

-- Utility -------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
