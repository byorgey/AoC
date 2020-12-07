import           Control.Arrow
import           Data.Char
import           Data.List.Split
import           Data.Map        (Map, (!))
import qualified Data.Map        as M
import qualified Data.Set        as S

type Passport = Map String String

main = interact $
  splitOn "\n\n" >>>
  map (words >>> map (splitOn ":" >>> (\[k,v] -> (k,v))) >>> M.fromList) >>>
  applyAll [solveA, solveB] >>>
  map show >>> unlines

passportFields = S.fromList $ words "byr iyr eyr hgt hcl ecl pid cid"
requiredFields = S.delete "cid" passportFields

solveA, solveB :: [Passport] -> Int
solveA = count hasRequiredFields
solveB = count valid

hasRequiredFields :: Passport -> Bool
hasRequiredFields pp = requiredFields `S.isSubsetOf` (M.keysSet pp)

valid :: Passport -> Bool
valid pp = hasRequiredFields pp && all validateField requiredFields
  where
    validateField f = validate f (pp!f)

    validate "byr" x = validateNumber 1920 2002 x
    validate "iyr" x = validateNumber 2010 2020 x
    validate "eyr" x = validateNumber 2020 2030 x
    validate "hgt" x = case span isDigit x of
      (n, "cm") -> validateNumber 150 193 n
      (n, "in") -> validateNumber 59 76 n
      _         -> False
    validate "hcl" ('#':n) = all (\c -> isHexDigit c && not (isUpper c)) n
    validate "ecl" x = x `elem` allowedEyeColors
    validate "pid" x = all isDigit x && length x == 9

    validate _ _ = False

    validateNumber _ _ ""  = False
    validateNumber lo hi x = all isDigit x && (read x `inRange` (lo, hi))

    allowedEyeColors = words "amb blu brn gry grn hzl oth"

inRange x (lo,hi) = lo <= x && x <= hi
count p = filter p >>> length
applyAll fs x = map ($x) fs
