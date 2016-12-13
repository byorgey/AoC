subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (a:as) = map (a:) s' ++ s'
  where
    s' = subsets as

main = do
  ns <- (map read . lines) <$> getContents
  let goodSubsets = filter ((==150) . sum) . subsets $ ns
  print $ length goodSubsets

  let min = minimum . map length $ goodSubsets
  print . length . filter ((==min) . length) $ goodSubsets
