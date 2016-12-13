import           Control.Arrow
import           Data.List
import qualified Data.Set      as S

data Rule = Rule String String

readRule :: String -> Rule
readRule = (Rule <$> (!!0) <*> (!!2)) . words

readInput :: String -> ([Rule], String)
readInput
  = (map readRule *** (!!1))
  . span (not.null)
  . lines

replacements :: String -> Rule -> [String]
replacements "" _ = [""]
replacements str@(x:xs) r@(Rule from to)
  | from `isPrefixOf` str = (to ++ drop (length from) str) : map (x:) (replacements xs r)
  | otherwise = map (x:) (replacements xs r)

bfs :: Ord a => (a -> Bool) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bfs isGoal next start = bfs' S.empty start
  where
    bfs' seen layer
      | any isGoal layer = [layer]
      | otherwise = layer : bfs' seen' layer'
        where
          layer' = (foldMap next layer) `S.difference` seen'
          seen' = S.union seen layer

main = do
  (rs,s) <- readInput <$> getContents
  print . pred . S.size . S.unions . map S.fromList . map (replacements s) $ rs

  let len = length s

  mapM_ print . map length
    $ bfs (==s) (\m -> S.unions $ map (S.filter ((<=len) . length) . S.fromList . replacements m) rs) (S.singleton "e")
