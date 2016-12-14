import           System.Environment

{-

  diag = (r+c) - 2   (diags numbered from 0)

  start of diag d = 1 + triangle(d) = 1 + d*(d+1)/2

  index within diag = column-1.
-}

f c = (c * 252533) `mod` 33554393

main = do
  args <- map read <$> getArgs
  let [r,c] = case args of
                [] -> [2981, 3075]
                _  -> args
  let d = r+c - 2
      ix = (d*(d+1) `div` 2 + c)
      code0 = 20151125

      code = iterate f code0 !! (ix-1)

  print code
