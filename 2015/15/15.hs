import           Control.Monad
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe

type Quality = String

type Ingredient = M.Map Quality Int

readIngredient :: String -> Ingredient
readIngredient
  = M.fromList . map (\[q,i] -> (q,read i)) . chunksOf 2 . tail . words . filter (/=',')

readIngredients = map readIngredient . lines

-- recipes k n gives ordered partitions of n into k parts
recipes :: Int -> Int -> [[Int]]
recipes k 0 = [replicate k 0]
recipes 0 _ = []
recipes k n = [ p : ps | p <- [0 .. n], ps <- recipes (k-1) (n-p) ]

recipeScore :: [Ingredient] -> [Int] -> Int
recipeScore is r
  = product . M.elems . M.delete "calories"
  $ max 0 <$> (M.unionsWith (+) $ zipWith (\m tsp -> (*tsp) <$> m) is r)

recipeScore2 :: [Ingredient] -> [Int] -> Maybe Int
recipeScore2 is r = do
  let totalScore = max 0 <$> (M.unionsWith (+) $ zipWith (\m tsp -> (*tsp) <$> m) is r)
  guard (totalScore M.! "calories" == 500)
  return . product . M.elems . M.delete "calories" $ totalScore


main = do
  ingrs <- readIngredients <$> getContents
  let rs = recipes (length ingrs) 100
  print . maximum $ map (recipeScore ingrs) rs

  print . maximum . catMaybes $ map (recipeScore2 ingrs) rs
