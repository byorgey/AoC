#!/usr/bin/env stack
-- stack --resolver lts-19.28 script --package containers --package split --package megaparsec

import           Control.Arrow              ((>>>))
import           Data.List                  (findIndex, sort)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromJust)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal)

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

------------------------------------------------------------

data Packet = PInt Int | PList [Packet] deriving (Eq, Show)
instance Ord Packet where
  compare (PInt x) (PInt y)     = compare x y
  compare (PList xs) (PList ys) = compare xs ys
  compare (PInt x) y            = compare (PList [PInt x]) y
  compare x (PInt y)            = compare x (PList [PInt y])

type Input = [[Packet]]

readInput :: String -> Input
readInput = lines >>> splitOn [""] >>> map (map readPacket)

type Parser = Parsec Void String

parsePacket :: Parser Packet
parsePacket =
      PInt <$> decimal
  <|> PList <$> (char '[' *> parsePacket `sepBy` char ',' <* char ']')

readPacket = readParser parsePacket
readParser p = parse p "" >>> either undefined id

------------------------------------------------------------

type Output = Int

solveA, solveB :: Input -> Output
solveA = findAllIndex (\[x,y] -> x < y) >>> map succ >>> sum
solveB = concat >>> (dividers++) >>> sort >>> applyAll (map (findIndex' . (==)) dividers) >>> map succ >>> product

dividers = map readPacket ["[[2]]", "[[6]]"]

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p = findIndex p >>> fromJust

findAllIndex :: (a -> Bool) -> [a] -> [Int]
findAllIndex p = zip [0..] >>> filter (snd >>> p) >>> map fst
