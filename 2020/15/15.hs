{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as V
import           System.IO.Unsafe

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [Int]

readInput :: String -> Input
readInput = splitOn "," >>> map read

type Output = Int

solveANaive, solveA, solveB :: Input -> Output
solveANaive = reverse >>> iterate stepNaive >>> find (length >>> (==2020)) >>> fromJust >>> head
solveA start = unsafePerformIO (stepTo start 2020)
solveB start = unsafePerformIO (stepTo start 30000000)

stepNaive :: [Int] -> [Int]
stepNaive xxs@(x:xs)
  | x `notElem` xs = 0:xxs
  | otherwise   = (xs >$> takeWhile (/= x) >>> length >>> succ) : xxs

-- Not pretty but it works!
stepTo :: [Int] -> Int -> IO Int
stepTo start n = do
  l <- V.replicate n (0 :: Int)

  forM_ (zip (init start) [1 :: Int ..]) $ \(x,t) -> V.write l x t

  step l (length start) (last start)

  where
    -- Take a step: at time t we are about to speak the given number.
    step l t toSpeak
      | t == n     = return toSpeak
      | otherwise = do

          -- When was the last time this number was spoken?
          t' <- V.read l toSpeak

          -- Record this occurrence.
          V.write l toSpeak t

          -- Then decide what to speak next.
          case t' of
            0 -> step l (t+1) 0
            _ -> step l (t+1) (t - t')

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
