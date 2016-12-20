{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Data.IntervalSet as IS
import           Data.List.Split
import           Data.Word

data I = I Word32 Word32
  deriving (Show, Eq, Ord)

instance IS.Interval I Word32 where
  lowerBound (I l _) = l
  upperBound (I _ u) = u

readInterval :: String -> I
readInterval s = let [x,y] = splitOn "-" s in I (read x) (read y)

main = do
  is <- (map readInterval . lines) <$> getContents

  let iset = foldr IS.insert IS.empty is

  print $ findFirstUnused iset 0

  print $ findNumAllowed iset

findFirstUnused iset e = go e
  where
    go n
      = case IS.findMax (IS.containing iset n) of
          Nothing -> n
          Just i  -> let u = IS.upperBound i
                     in  if u == (maxBound :: Word32) then u else go (u + 1)

findLastUnused :: IS.IntervalSet I -> Word32 -> Word32
findLastUnused iset e
  = case IS.splitAt iset e of
      (_,_,iset') -> case IS.findMin iset' of
        Nothing -> (maxBound :: Word32)
        Just i  -> IS.lowerBound i

findNumAllowed :: IS.IntervalSet I -> Word32
findNumAllowed iset = go 0
  where
    go n =
      let b = findFirstUnused iset n
          e = findLastUnused iset b
      in  (e-b) + if (b == maxBound) then 0 else go e
