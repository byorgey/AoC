import           Data.List.Split
import qualified Data.Map        as M

data Sue = Sue Int (M.Map String Int)
  deriving Show

readSue :: String -> Sue
readSue = buildSue . tail . words . filter (`notElem` ":,")
  where
    buildSue (num:attrs) = Sue (read num) (M.fromList attrList)
      where
        attrList = map (\[x,y] -> (x,read y)) $ chunksOf 2 attrs

theSue :: Sue
theSue = Sue 0 . M.fromList $
  [ ("children", 3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 2)
  , ("perfumes", 1)
  ]

subSue :: Sue -> Sue -> Bool
subSue (Sue _ as) (Sue _ bs) =
  all (\(a,v) -> M.lookup a bs == Just v) (M.assocs as)

subSue2 :: Sue -> Sue -> Bool
subSue2 (Sue _ as) (Sue _ bs) =
  all (\(a,v) -> compat a (M.lookup a bs) (Just v)) (M.assocs as)
  where
    compat attr
      | attr `elem` ["cats", "trees"] = (<)
      | attr `elem` ["pomeranians", "goldfish"] = (>)
      | otherwise = (==)

main = do
  sues <- (map readSue . lines) <$> getContents

  print . filter (`subSue` theSue) $ sues
  print . filter (`subSue2` theSue) $ sues
