{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Arrow   ((***))
import           Data.Bits       (xor)
import           Data.List
import           Data.List.Split

input = "01000100010010111"

-- | An abstract algebraic representation of a bitstring, with the
--   length and xor of the entire bitstring cached at every node.
data BitString where
  -- | The empty bitstring.
  Emp :: BitString

  -- | A single bit.
  Bit :: Bool -> BitString

  -- | Append two bitstrings.
  App :: !Bool -> !Integer -> BitString -> BitString -> BitString

  -- | Invert a bitstring.
  Inv :: !Bool -> !Integer -> BitString -> BitString

  -- | Reverse a bitstring.
  Rev :: !Bool -> !Integer -> BitString -> BitString

  -- | The "dragon" transformation, which sends s to  s ++ 0 ++ invert (reverse s).
  Drg :: !Bool -> !Integer -> BitString -> BitString

  deriving Show

-- | Extract the (cached) length of a bitstring.
bsLen :: BitString -> Integer
bsLen Emp = 0
bsLen (Bit _) = 1
bsLen (App _ l _ _) = l
bsLen (Inv _ l _)   = l
bsLen (Rev _ l _)   = l
bsLen (Drg _ l _)   = l

-- | Extract the (cached) xor of a bitstring.
bsXor :: BitString -> Bool
bsXor Emp     = False
bsXor (Bit b) = b
bsXor (App b _ _ _) = b
bsXor (Inv b _ _)   = b
bsXor (Rev b _ _)   = b
bsXor (Drg b _ _)   = b

-- Smart constructors. These take care of properly maintaining the
-- cached length and xor.

-- | A single bit.
bit = Bit

-- | A literal string of bits, represented as a string of '0' and '1'
--   for ease of input.
bits :: String -> BitString
bits = foldr (app . bit . (=='1')) Emp

-- | Append two bitstrings.
app :: BitString -> BitString -> BitString
app s1 Emp = s1
app s1 s2 = App (bsXor s1 `xor` bsXor s2) (bsLen s1 + bsLen s2) s1 s2

-- | Invert a bitstring.
inv :: BitString -> BitString
inv s = Inv (if even (bsLen s) then bsXor s else not (bsXor s))
            (bsLen s)
            s
  -- Inverting a string preserves the xor when s has even length, and
  -- inverts the xor when s has odd length.

-- | Reverse a bitstring.
rev :: BitString -> BitString
rev s = Rev (bsXor s) (bsLen s) s

-- | Apply the dragon transform to a bitstring.
dragon :: BitString -> BitString
dragon s = Drg (bsXor s `xor` bsXor (inv s)) (2*(bsLen s) + 1) s

-- | Reverse a bitstring, pushing the operation through one level of
--   structure so the result does not have a 'Rev' node at the top (or
--   at least has fewer total Rev nodes).
pushRev :: BitString -> BitString
pushRev Emp     = Emp
pushRev (Bit b) = Bit b
pushRev (App _ _ s1 s2) = rev s2 `app` rev s1
pushRev (Inv _ _ s) = inv (rev s)
pushRev (Rev _ _ s) = s
pushRev (Drg _ _ s) = dragon (inv s)

-- | Apply the dragon transformation, but do it in terms of other,
--   more primitive BitString constructors instead of using the Dragon
--   constructor.
expandDragon :: BitString -> BitString
expandDragon s = s `app` (bit False `app` inv (rev s))

-- Splitting, filling, and checksumming

-- | The workhorse of the implementation.  This is analogous to
--   'splitAt' for strings: given a length and a BitString, split it
--   into two parts, the first having the given length and the second
--   containing the rest.
splitBits :: Integer -> BitString -> (BitString, BitString)
splitBits 0 s                = (Emp, s)
splitBits n s | n >= bsLen s = (s, Emp)
splitBits n (App _ _ s1 s2)
  | n < bsLen s1 = let (s1a, s1b) = splitBits n s1 in (s1a, s1b `app` s2)
  | otherwise    = let (s2a, s2b) = splitBits (n - bsLen s1) s2 in (s1 `app` s2a, s2b)
splitBits n (Inv _ _ s) = (inv *** inv) $ splitBits n s
splitBits n (Rev _ _ s) = splitBits n (pushRev s)
splitBits n (Drg _ _ s) = splitBits n (expandDragon s)

-- | Perform the "fill" operation: keep applying the dragon transform
--   until we have enough bits, then take the required number.
fill :: Integer -> String -> BitString
fill n str = fst . splitBits n $ go (bits str)
  where
    go s | bsLen s >= n = s
         | otherwise    = go (dragon s)

-- | If n = m * 2^k where m is odd, then each block of length 2^k will
--   turn into a single bit of the output checksum (in particular, the
--   negation of its xor).  So compute 2^k, split the bitstring into
--   blocks of that length, and find the nxor of each block.
checksum :: BitString -> [Bool]
checksum s = map (not . bsXor) . unfoldr doSplit $ s
  where
    doSplit Emp = Nothing
    doSplit s   = Just (splitBits blockLen s)
    blockLen = powTwo (bsLen s)
    powTwo n
      | odd n     = 1
      | otherwise = 2 * powTwo (n `div` 2)

-- | Convert a BitString to an actual list of bits, for testing
--   purposes.
toBits :: BitString -> [Bool]
toBits Emp = []
toBits (Bit b) = [b]
toBits (App _ _ s1 s2) = toBits s1 ++ toBits s2
toBits (Inv _ _ s) = map not (toBits s)
toBits (Rev _ _ s) = reverse (toBits s)
toBits (Drg _ _ s) = toBits (expandDragon s)

-- | Display a string of bits.
showBits :: [Bool] -> String
showBits = map (\b -> if b then '1' else '0')

main = do
  -- Solve both parts.  These finish instantly.
  putStrLn . showBits $ checksum (fill 272 input)
  putStrLn . showBits $ checksum (fill 35651584 input)

  -- Checksumming 34 megabytes of data is nothing.  How about 34
  -- YOTTABYTES.  This also finishes instantly.  And it turns out this
  -- has the *same* checksum as part 2.  The checksums for (17 * 2^n)
  -- appear to have a cycle of period 12.
  putStrLn . showBits $ checksum (fill (17 * 2^81) input)

  putStrLn . showBits $ checksum (fill 20551738933448695970004992 input)

----------------------------------------------------------------------
-- Brute-force approach used for part 1
----------------------------------------------------------------------

-- dragon a = a ++ "0" ++ reverse (map inv a)
--   where
--     inv '0' = '1'
--     inv '1' = '0'

-- fill len = take len . fromJust . find ((>= len) . length) . iterate dragon

-- chk1 = map xor' . chunksOf 2
--   where
--     xor' [x,y] | x == y = '1'
--                | otherwise = '0'

-- checksum a
--   | odd (length a) = a
--   | otherwise = checksum . map xor' . chunksOf 2 $ a
--   where
--     xor' [x,y] | x == y = '1'
--                | otherwise = '0'

-- main = do
--   print . (!!4) . iterate chk1 $ fill 272 input
