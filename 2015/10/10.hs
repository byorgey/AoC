lookSay :: [Int] -> [Int]
lookSay []     = []
lookSay (d:ds) =
  let (reps, rest) = span (==d) ds
  in  (1 + length reps) : d : lookSay rest

main = do
  let start = map (read . (:[])) "3113322113"
  print . length $ (iterate lookSay start !! 40)
  print . length $ (iterate lookSay start !! 50)
