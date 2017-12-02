input = 3014603

------------------------------------------------------------
-- Part 1
------------------------------------------------------------

josephus n = 2*(n - 2^m) + 1
  where
    m = ilog 2 n

-- for n>0, ilog k n = m iff m is largest such that k^m <= n
ilog k n | n < k = 0
ilog k n = 1 + ilog k (n `div` k)

------------------------------------------------------------
-- Part 2
------------------------------------------------------------

-- Brute force solution to part 2, way too slow, but good enough to
-- notice & conjecture a general formula
step :: [Int] -> [Int]
step (x:xs) = xs' ++ [x]
  where
    (xs1, _ : xs2) = splitAt ((length xs - 1) `div` 2) xs
    xs' = xs1 ++ xs2

play :: [Int] -> Int
play xs = head . (!!(length xs - 1)) . iterate step $ xs

solve :: Int -> Int
solve n = play [1..n]

-- > map solve [1..81]
-- [1,1,3,1,2,3,5,7,9,1,2,3,4,5,6,7,8,9,11,13,15,17,19,21,23,25,27,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81]
--
-- [1] ++
-- [1] ++ [3] ++
-- [1..3] ++ [5,7,9] ++
-- [1..9] ++ [11,13 .. 27] ++
-- [1..27] ++ [29,31 .. 81] ++
-- ... and so on

josephus2 1 = 1
josephus2 n
  | n' <= 3^k = n'
  | otherwise = 3^k + 2*(n' - 3^k)
  where
    k = ilog 3 (n - 1)
    n' = n - 3^k

-- > and (zipWith (==) (map josephus2 [1..1000]) (map solve [1..1000]))
-- True

main = do
  print (josephus input)
  print (josephus2 input)
