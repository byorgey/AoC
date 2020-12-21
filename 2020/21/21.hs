{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

import           Control.Arrow
import           Data.Char       (isAlpha, isSpace)
import           Data.List
import           Data.List.Split
import           Data.Map        (Map, (!))
import qualified Data.Map        as M
import           Data.Set        (Set)
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> unlines

data Food = Food { ingredients :: [String], allergens :: [String] }
type Input = [Food]

readInput :: String -> Input
readInput = lines >>> map readFood
  where
    readFood = filter ((||) <$> isAlpha <*> isSpace) >>> words >>> splitOn ["contains"] >>>
      (\[is,as] -> Food is as)

type Output = String

byAllergen :: Food -> [(String, Set String)]
byAllergen (Food is as) = map (, S.fromList is) as

allergenMap :: Input -> Map String String
allergenMap = concatMap byAllergen >>> M.fromListWith S.intersection >>> refine

refine :: Map String (Set String) -> Map String String
refine m
  | M.null m  = M.empty
  | otherwise = M.insert solvedAllergen solvedIngredient (refine m')
  where
    Just (solvedAllergen, S.toList -> [solvedIngredient])
      = find (snd >>> S.size >>> (==1)) (M.assocs m)

    m' = M.map (S.delete solvedIngredient) (M.delete solvedAllergen m)

solveA, solveB :: Input -> Output
solveA fs = fs >$> concatMap ingredients >>> filter (`S.notMember` known) >>> length >>> show
  where
    known = fs >$> allergenMap >>> M.elems >>> S.fromList

solveB = allergenMap >>> M.elems >>> intercalate ","

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)
