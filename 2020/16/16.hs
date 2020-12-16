{-# LANGUAGE RecordWildCards #-}

import           Control.Arrow
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

------------------------------------------------------------

data Input = Input { fields :: [Field], mine :: Ticket, nearby :: [Ticket] }
  deriving Show
type Ticket = [Int]
data Field = Field { fieldName :: String, rule :: Rule } deriving (Show, Eq, Ord)
type Rule = [Range]
type Range = (Int,Int)

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>>
  (\[fs, [_,t], (_:ts)] -> Input (map readField fs) (readTicket t) (map readTicket ts))

readField :: String -> Field
readField = splitOn ":" >>>
  (\[nm, rs] -> Field nm (map readRule (rs >$> words >>> filter (/="or"))))

readRule :: String -> Range
readRule = splitOn "-" >>> map read >>> (\[a,b] -> (a,b))

readTicket :: String -> Ticket
readTicket = splitOn "," >>> map read

type Output = Int

------------------------------------------------------------

inRange :: Range -> Int -> Bool
inRange (lo,hi) x = lo <= x && x <= hi

matches :: Rule -> Int -> Bool
matches rs x = any (`inRange` x) rs

matchesF :: Field -> Int -> Bool
matchesF f = matches (rule f)

valid :: [Field] -> Ticket -> Bool
valid fs = all $ \x -> any (\r -> matches r x) (map rule fs)

------------------------------------------------------------

solveA, solveB :: Input -> Output
solveA (Input{..}) =
  concat nearby >$>
  filter (\x -> all (\r -> not (matches r x)) (map rule fields)) >>> sum
solveB (Input{..}) = departureProduct
  where
    validTickets = filter (valid fields) (mine : nearby)
    possibleFieldsFor :: [Int] -> Set Field
    possibleFieldsFor xs = S.fromList $ filter (\f -> all (matchesF f) xs) fields

    possibleFields = map possibleFieldsFor (transpose validTickets)

    theFields = possibleFields >$>
      iterate refine >>>
      find (all (S.size >>> (==1))) >>>
      fromJust >>> map S.toList >>> concat

    departureProduct = mine >$>
      zip theFields >>>
      filter (fst >>> fieldName >>> ("departure" `isPrefixOf`)) >>>
      map snd >>> product

refine :: [Set Field] -> [Set Field]
refine potentialFields = map winnow potentialFields
  where
    winnow fs
      | S.size fs == 1 = fs
      | otherwise      = fs `S.difference` fixed

    fixed = filter (S.size >>> (==1)) potentialFields >$> S.unions

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

infixr 0 >$>
(>$>) = flip ($)

