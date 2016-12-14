{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}

import           Control.Lens
import           Control.Monad.Writer
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           System.Environment
import           Text.Printf

data Item = Item
  { name   :: String
  , cost   :: Int
  , damage :: Int
  , armor  :: Int
  }
  deriving Show

readItem :: String -> Item
readItem s = Item n c d a
  where
    [n, read -> c, read -> d, read -> a] = words s

readStore :: String -> [[Item]]
readStore = map (map readItem . tail) . splitOn [""] . lines

type HP = Int

data Player = Player
  { _playerName   :: String
  , _playerHP     :: HP
  , _playerDamage :: Int
  , _playerArmor  :: Int
  }
  deriving Show

makeLenses ''Player

readPlayer :: String -> String -> Player
readPlayer nm s = Player nm hp d a
  where
    [hp, d, a] = map (read . last . words) . lines $ s

equip :: [Item] -> Player -> Player
equip items p =
  p & playerDamage +~ sum (map damage items)
    & playerArmor  +~ sum (map armor  items)

-- Returns True iff p1 wins, and logs the events.
simulate :: Player -> Player -> Writer [String] Bool
simulate p1@(Player p1n _ d _) p2@(Player p2n hp _ a) = do
  tell [printf "%s does %d - %d = %d damage; %s is down to %d HP." p1n d a dTot p2n hp']
  case hp' <= 0 of
    True -> return True
    _    -> not <$> simulate (p2 & playerHP .~ hp') p1
  where
    dTot = max 1 (d - a)
    hp'  = hp - dTot

-- First argument is player file; next is boss file.

main = do
  (pfile : bfile : useItems) <- getArgs
  store <- readStore <$> readFile "store.txt"
  player <- readPlayer "The player" <$> readFile pfile
  boss   <- readPlayer "The boss"   <$> readFile bfile

  -- printSimulation player boss

  -- Part 1
  let cheapestWin = fromJust . find (canWinWith player boss) . itemSelections $ store
  print cheapestWin
  print . sum . map cost $ cheapestWin
  printSimulation (equip cheapestWin player) boss

  -- Part 2
  let dearestLoss = fromJust . find (not . canWinWith player boss) . reverse . itemSelections $ store
  print dearestLoss
  print . sum . map cost $ dearestLoss
  printSimulation (equip dearestLoss player) boss

itemSelections :: [[Item]] -> [[Item]]
itemSelections [weapons, armor, rings]
  = sortBy (comparing (sum . map cost)) $
    [ [w] ++ a ++ r | w <- weapons
                    , a <- [] : map (:[]) armor
                    , r <- [] : choose 1 rings ++ choose 2 rings ]

choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

canWinWith :: Player -> Player -> [Item] -> Bool
canWinWith player boss items = fst $ runWriter (simulate (equip items player) boss)

printSimulation :: Player -> Player -> IO ()
printSimulation player boss = do
  let (win, log) = runWriter (simulate player boss)

  mapM_ putStrLn log

  case win of
    True  -> putStrLn "You win!"
    False -> putStrLn "You died."
