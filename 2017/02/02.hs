{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Arrow

-- main = interact $
--   lines >>> map (words >>> map read >>> (maximum &&& minimum) >>> uncurry (-)) >>> sum >>> show

main = interact $
  lines >>> map (words >>> map read >>> findDiv) >>> sum >>> show

findDiv :: [Integer] -> Integer
findDiv = pairs >>> filter (\(a,b) -> a `mod` b == 0 || b `mod` a == 0) >>> head
  >>> (\(a,b) -> if a > b then a `div` b else b `div` a)

pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs
