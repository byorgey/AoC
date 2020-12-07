module Util where

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

runAll :: [a -> b] -> a -> [b]
runAll fs a = map ($a) fs
