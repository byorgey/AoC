{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.List

data Disc = Disc
  { _discSize :: Int
  , _position :: Int
  }
  deriving Show

makeLenses ''Disc

readDisc :: String -> Disc
readDisc s = Disc (read $ s' !! 3) (read . init $ s' !! 11)
  where
    s' = words s

egcd :: Int -> Int -> (Int,Int,Int)
egcd a b = go (a,1,0) (b,0,1)
  where
    go g (0,_,_) = g
    go (a,ax,ay) (b,bx,by) = go (b,bx,by) (a `mod` b, ax - (a `div` b) * bx
                                                    , ay - (a `div` b) * by)

meld :: Disc -> Disc -> Disc
meld (Disc n1 a1) (Disc n2 a2) = Disc (n1*n2) a
  where
    (g,m1,m2) = egcd n1 n2
    a = (a1*m2*n2 + a2*m1*n1) `mod` (n1*n2)

crt :: [Disc] -> Disc
crt = foldl1' meld

main = do
  discs <- (map readDisc . lines) <$> getContents

  let wantedConfig  = crt $ zipWith (\d p -> d & position .~ p) discs [-1, -2 ..]
      currentConfig = crt discs

  print ((wantedConfig ^. position - currentConfig ^. position) `mod` (currentConfig ^. discSize))

