{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bits
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set               as S
import           Data.Tuple
import           System.Environment
import           Text.Parsec            hiding (State)
import           Text.Parsec.String
import           Text.Printf

main = do
  ls <- (map read . splitOn ",") <$> getContents

  -- Part 1
  let s = go 0 0 ls id
  print (s 0 * s 1)

  -- Part 2

n = 256

go :: Int -> Int -> [Int] -> Str -> Str
go _ _ [] = id
go pos skip (l:ls) = go ((pos + l + skip) `mod` n) (skip+1) ls . rev pos l

type Str = Int -> Int

rev :: Int -> Int -> Str -> Str
rev i len s = \j -> if inSpan i len j then s (revSpan i len j) else s j

inSpan :: Int -> Int -> Int -> Bool
inSpan i len j = (if i <= i' then (&&) else (||)) (i <= j) (j < i')
  where
    i' = (i + len) `mod` n

revSpan :: Int -> Int -> Int -> Int
revSpan i len j = (i + (len - 1 - (j - i))) `mod` n

