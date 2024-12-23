{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bits
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set               as S
import           Data.Tuple
import           System.Environment
import           Text.Parsec            hiding (State)
import           Text.Parsec.String
import           Text.Printf

data Thing = Thing
  deriving Show

readThing :: String -> Thing
readThing _ = Thing

main = do
  n <- read <$> getContents
  let (x,y) = toCoords n

  print (abs x + abs y)

toCoords :: Integer -> (Integer, Integer)
toCoords n = (x,y)
  where
    s :: Integer
    s = floor (sqrt (fromIntegral n))
    so | even s    = s - 1
       | otherwise = s
    (sox, soy) = (so `div` 2 + 1, -so `div` 2 - 1)
    off = n - (so^2)
    (x,y)
      | off == 0  = (sox - 1, soy + 1)
      | otherwise = case off `div` (so+1) of
          0 -> (sox, soy + off `mod` (so+1))
          1 -> (sox - (off `mod` (so+1)), -soy)
          2 -> (-sox, -soy - off `mod` (so+1))
          3 -> (-sox + (off `mod` (so+1)), soy)

------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

dfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> a -> [[a]]
dfs winning fnext start = dfs' S.empty [start] start
  where
    dfs' visited path cur
      | winning cur = [path]
      | otherwise = concatMap (\n -> dfs' (S.insert n visited) (n:path) n) next
        where
          next = fnext cur

choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

readParser :: Parser a -> String -> a
readParser p s = case runParser p () "" s of
  Left err -> error (show err)
  Right a  -> a

int :: Parser Int
int = read <$> many1 digit

type Loc = (Int,Int)

addLoc :: Loc -> Loc -> Loc
addLoc (x1,y1) (x2,y2) = (x1+x2, y1+y2)
