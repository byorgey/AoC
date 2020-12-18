{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Comonad.Sheet hiding (count)

import           Control.Applicative
import           Control.Arrow         ((>>>))
import           Data.Array
import           Data.Bits
import           Data.Char
import           Prelude               hiding (insert, take)
-- import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Tuple
import           Text.Printf

import           Debug.Trace

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [[Cell]]

readInput :: String -> Input
readInput = lines >>> map (map readCell)

readCell '#' = O
readCell '.' = X

type Output = Int

solveA, solveB :: Input -> Output
solveA = life >>> take (rightBy 1 & belowBy 1 & outwardBy 1 & kataBy 1) >>>
  map (map (map (count (==O)) >>> sum) >>> sum) >>> sum
solveB = const 0

------------------------------------------------------------

-- Adapted from https://github.com/kwf/ComonadSheet

data Cell = X | O deriving ( Eq , Show )
type Universe = Sheet4 Cell

life :: [[Cell]] -> Universe
life seed = evaluate $ insert [[map (map const) seed]] blank
   where blank = sheet (const X) (repeat . tapeOf . tapeOf . tapeOf $ rule)
         rule place  = case neighbors place of
                            3 -> O
                            2 -> cell ana place
                            _ -> X
         neighbors   = length . filter (O ==) . cells bordering
         bordering   = map (ana &) (alongs ++ plane ++ ((&) <$> alongs <*> plane))
         plane       = diagonals ++ verticals ++ horizontals
         diagonals   = (&) <$> horizontals <*> verticals
         alongs      =        [inward, outward]
         verticals   = map d3 [above, below]
         horizontals = map d3 [right, left]

------------------------------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
