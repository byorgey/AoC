{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Data.Bits
import           Data.Function

factorA = 16807
factorB = 48271

startA = 591
startB = 393

m = 2147483647

values :: Int -> Int -> Int -> [Int]
values f k = filter ((==0) . (`mod` k)) . iterate ((`mod` m) . (*f))

main = do
  print (length . filter id . take 40000000 . tail $ zipWith ((==) `on` (.&. 0xffff)) (values factorA 1 startA) (values factorB 1 startB))

  print (length . filter id . take 5000000 . tail $ zipWith ((==) `on` (.&. 0xffff)) (values factorA 4 startA) (values factorB 8 startB))
