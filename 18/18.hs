import           Data.List

type Row = String

nextRow :: Row -> Row
nextRow = map rule . takeWhile ((==3).length) . map (take 3) . tails . ('.':) . (++".")
  where
    rule "^.." = '^'
    rule "..^" = '^'
    rule ".^^" = '^'
    rule "^^." = '^'
    rule _     = '.'

main = do
  [x] <- lines <$> getContents

  print . length . filter (=='.') . concat . take 40 $ iterate nextRow x

  print . length . filter (=='.') . concat . take 400000 $ iterate nextRow x
