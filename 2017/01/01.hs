import           Data.Char

main = do
  s <- filter isDigit <$> readFile "input"
  let (s1,s2) = splitAt (length s `div` 2) s
  print (sum . map (read . (:[]) . fst) . filter (uncurry (==)) $ zip s (s2 ++ s1))
