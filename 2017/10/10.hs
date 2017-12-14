{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- import           Control.Applicative
-- import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
-- import           Control.Monad.Writer
import           Data.Bits
-- import           Data.ByteString.Char8 (pack, unpack)
import           Data.Char
-- import           Data.Function
import           Data.List
import           Data.List.Split
-- import qualified Data.Map              as M
-- import           Data.Maybe
-- import           Data.Ord
-- import qualified Data.Set              as S
-- import           Data.Tuple
-- import           System.Environment
import           Text.Printf


n = 256

type Str = Int -> Int


data St = St
  { _pos  :: Int
  , _skip :: Int
  , _str  :: Str
  }

makeLenses ''St

initSt :: St
initSt = St 0 0 id

step :: Int -> State St ()
step l = do
  p <- use pos
  s <- use skip
  str %= rev p l
  pos %= ((`mod` n) . (+s) . (+l))
  skip += 1

-- go :: Int -> Int -> [Int] -> Str -> Str
-- go _ _ [] = id
-- go pos skip (l:ls) = go ((pos + l + skip) `mod` n) (skip+1) ls . rev pos l

rev :: Int -> Int -> Str -> Str
rev i len s = \j -> if inSpan i len j then s (revSpan i len j) else s j

inSpan :: Int -> Int -> Int -> Bool
inSpan i len j = (if i <= i' then (&&) else (||)) (i <= j) (j < i')
  where
    i' = (i + len) `mod` n

revSpan :: Int -> Int -> Int -> Int
revSpan i len j = (i + (len - 1 - (j - i))) `mod` n

main = do
  raw <- init <$> getContents
  let ls1 = (map read . splitOn ",") raw
      ls2 = map ord raw ++ [17,31,73,47,23]

  -- Part 1
  let s = view str $ execState (mapM_ step ls1) initSt
  print (s 0 * s 1)

  -- Part 2
  let sparseHash = view str $ execState (replicateM 64 (mapM_ step ls2)) initSt
      denseHash = concatMap (printf "%02x" . foldl' xor 0) . chunksOf 16
                $ map sparseHash [0..n-1]

  putStrLn denseHash
