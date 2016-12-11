{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Char
import           Data.HashMap.Strict        (elems)
import           Data.Scientific

main = do
  f <- getContents
  print . sum . map read . words . map (\c -> if (c == '-' || isDigit c) then c else ' ') $ f

  let Just j = decode (pack f) :: Maybe Value

  print (getNonRedSum j)

getNonRedSum :: Value -> Integer
getNonRedSum (Object v)
  | any (== String "red") (elems v) = 0
  | otherwise = sum (map getNonRedSum (elems v))
getNonRedSum (Array v)  = sum (fmap getNonRedSum v)
getNonRedSum (String _) = 0
getNonRedSum (Number s) = case (floatingOrInteger s) of Right i -> i
getNonRedSum (Bool _)   = 0
getNonRedSum Null       = 0
