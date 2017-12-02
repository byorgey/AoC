{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import qualified Data.Set               as S

data Dir = U | D | L | R
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

input = "udskfozm"
-- input = "hijkl"
-- input = "ihgpwlah"
-- input = "kglvqrro"

type St = (Loc, [Dir])

isOpen :: St -> Dir -> Bool
isOpen (_,dirs) d = (h !! fromEnum d) >= 'b'
  where
    h = take 4 (mkHash (input ++ concatMap show dirs))

winning ((3,3),_) = True
winning _ = False

next :: St -> S.Set St
next ((3,3),_) = S.empty
next st@(loc, path)
  = S.fromList
  . filter (valid . fst)
  . map (\d -> (addLoc loc (toLDir d), path ++ [d]))
  . filter (isOpen st) $ [minBound .. maxBound]
  where
    toLDir U = (0,-1)
    toLDir D = (0,1)
    toLDir L = (-1,0)
    toLDir R = (1,0)
    valid (x,y) = x >= 0 && x < 4 && y >= 0 && y < 4

main = do
  putStrLn . concatMap show . snd . head . filter ((==(3,3)).fst) . S.toList . last $
    bfs winning next (S.singleton ((0,0),[]))

  print . pred . maximum . map length $ dfs winning next ((0,0),[])


------------------------------------------------------------
-- Utilities

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

dfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> a -> [[a]]
dfs winning fnext start = dfs' S.empty [start] start
  where
    dfs' visited path cur
      | winning cur = [path]
      | otherwise = concatMap (\n -> dfs' (S.insert n visited) (n:path) n) next
        where
          next = fnext cur

type Loc = (Int,Int)

addLoc :: Loc -> Loc -> Loc
addLoc (x1,y1) (x2,y2) = (x1+x2, y1+y2)

mkHash :: String -> String
mkHash = unpack . encode . hash . pack

