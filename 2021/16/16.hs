#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import           Control.Arrow        ((>>>))
import           Control.Monad        (replicateM)
import           Data.Bits            (shiftL)
import           Data.Char            (ord)
import           Data.List            (elemIndex, foldl')
import           Data.Maybe           (fromJust)
import qualified Numeric              as N
import           Text.Printf          (printf)

import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

main = interact $
  readInput >>> applyAll [versionSum, eval] >>> map show >>> unlines

------------------------------------------------------------
-- AST

data Packet
  = Literal { version :: Int, typeID :: Int, value :: Int }
  | Operator { version :: Int, typeID :: Int, children :: [Packet] }
  deriving Show

------------------------------------------------------------
-- Parsing

readInput :: String -> Packet
readInput = expandHex >>> parse parsePacket "" >>> either undefined id

expandHex :: String -> String
expandHex = concatMap (readHexDigit >>> printf "%04b")
  where
    readHexDigit d
      | '0' <= d && d <= '9' = ord d - ord '0'
      | otherwise            = 10 + ord d - ord 'A'

type Parser = Parsec Void String

bits :: Int -> Parser Int
bits n = readBase 2 "01" <$> replicateM n binDigitChar

bit :: Parser Int
bit = bits 1

parsePacket :: Parser Packet
parsePacket = do
  packetVersion <- bits 3
  packetTypeID  <- bits 3
  case packetTypeID of
    4 -> Literal packetVersion packetTypeID <$> parseLiteral
    _ -> Operator packetVersion packetTypeID <$> parseSubPackets

parseLiteral :: Parser Int
parseLiteral = foldl' (\n d -> n `shiftL` 4 + d) 0 <$> parseLiteralChunks

parseLiteralChunks :: Parser [Int]
parseLiteralChunks = do
  continue <- bit
  val <- bits 4
  case continue of
    0 -> return [val]
    1 -> (val:) <$> parseLiteralChunks

parseSubPackets :: Parser [Packet]
parseSubPackets = do
  lengthTypeID <- bit
  case lengthTypeID of
    0 -> bits 15 >>= parseSubPacketsByLength
    1 -> bits 11 >>= parseSubPacketsByCount

parseSubPacketsByLength :: Int -> Parser [Packet]
parseSubPacketsByLength len = do
  off <- getOffset
  parseSubPacketsUntil (off + len)

parseSubPacketsUntil :: Int -> Parser [Packet]
parseSubPacketsUntil end = do
  off <- getOffset
  if off >= end
    then return []
    else (:) <$> parsePacket <*> parseSubPacketsUntil end

parseSubPacketsByCount :: Int -> Parser [Packet]
parseSubPacketsByCount k = replicateM k parsePacket

------------------------------------------------------------
-- Evaluation

versionSum :: Packet -> Int
versionSum (Literal v _ _)   = v
versionSum (Operator v _ ps) = v + sum (map versionSum ps)

eval :: Packet -> Int
eval (Literal _ _ n) = n
eval (Operator _ t ps) = op t (map eval ps)
  where
    op 0 = sum
    op 1 = product
    op 2 = minimum
    op 3 = maximum
    op 5 = cmp (>)
    op 6 = cmp (<)
    op 7 = cmp (==)
    cmp (?) [x,y] = if x ? y then 1 else 0

------------------------------------------------------------
-- Utility

readBase :: Int -> [Char] -> String -> Int
readBase b digits s =
  case N.readInt b (`elem` digits) (flip elemIndex digits >>> fromJust) s of
    ((a,_):_) -> a

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

