{-# LANGUAGE RecordWildCards  #-}

import           Control.Arrow   ((>>>))
import           Data.Foldable   (asum)
import           Data.List       (find, partition, transpose)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as S

import           Scanner

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Board = [[Int]]
data Input = Input { calls :: [Int], boards :: [Board] }

readInput :: String -> Input
readInput = runScanner (Input <$> (map read . splitOn "," <$> str) <*> many (5 >< (5 >< int)))

type Output = Int

solveA, solveB :: Input -> Output
solveA = solveFor firstWin
solveB = solveFor lastWin

solveFor select Input{..} = c * sum (unmarked cs b)
  where
    (b, GameOutcome c cs _ _) = select (gameOutcomes calls boards)

------------------------------------------------------------

rows, cols :: Board -> [[Int]]
rows = id
cols = transpose

-- Has a board won yet, given the numbers that have been called?
wins :: Set Int -> Board -> Bool
wins called b = any win (rows b) || any win (cols b)
  where
    win = all (`S.member` called)

-- Get the unmarked numbers on a board.
unmarked :: Set Int -> Board -> [Int]
unmarked called = concat >>> filter (`S.notMember` called)

-- Given the numbers that have been called, partition the boards into
-- winning and not-winning.
winning :: Set Int -> [Board] -> ([Board], [Board])
winning called = partition (wins called)

-- Incrementally construct the sets of called numbers at each step.
calledSets :: [Int] -> [(Int, Set Int)]
calledSets calls = zip calls (drop 1 (scanl (flip S.insert) S.empty calls))

-- An outcome at a particular point during the game.
data GameOutcome = GameOutcome
  { lastCalled    :: Int
  , calledSet     :: Set Int
  , winningBoards :: [Board]
  , losingBoards  :: [Board]
  }

gameOutcomes :: [Int] -> [Board] -> [GameOutcome]
gameOutcomes called boards = map mkOutcome (calledSets called)
  where
    mkOutcome (c,cs) = uncurry (GameOutcome c cs) (winning cs boards)

firstWin :: [GameOutcome] -> (Board, GameOutcome)
firstWin gs = (head (winningBoards g), g)
  where
    g = find' (not . null . winningBoards) gs

lastWin :: [GameOutcome] -> (Board, GameOutcome)
lastWin gs = (head (losingBoards gLose), gWin)
  where
    gLose = find' ((==1) . length . losingBoards) gs
    gWin  = find' (null . losingBoards) gs

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

find' :: (a -> Bool) -> [a] -> a
find' p = find p >>> fromJust
