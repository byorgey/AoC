main = do
  f <- getContents

  -- This doesn't work because e.g.  "\xc6d" in Haskell is 1 Unicode character,
  --   but in the problem it's supposed to be 2 chars, \xc6 + d.  =(
  -- print . sum . map (\l -> length l - length (read l :: String)) $ lines f

  print . sum . map (\l -> length l - length (decode l)) $ lines f

  -- This does work though =)
  print . sum . map (\l -> length (show l) - length l) $ lines f

decode = go . init . tail
  where
    go ('\\' : '\\' : l) = '\\' : go l
    go ('\\' : '"'  : l) = '"' : go l
    go ('\\' : 'x' : a : b : l) = read ("\"\\x" ++ [a,b] ++ "\"") ++ go l
    go (x : l) = x : go l
    go [] = []
