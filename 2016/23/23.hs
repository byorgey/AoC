{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Data.Array

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as M
import           Data.Maybe
import           Text.Parsec          hiding (State)
import           Text.Parsec.String

import           Debug.Trace

type Reg = Char
data Val = V Int | R Reg deriving Show
data Instr
  = Cpy Val Val
  | Inc Val
  | Dec Val
  | Jnz Val Val
  | Tgl Val
  deriving Show

instruction :: Parser Instr
instruction =
      Cpy <$ string "cpy " <*> val <* char ' ' <*> val
  <|> Inc <$ string "inc " <*> val
  <|> Dec <$ string "dec " <*> val
  <|> Jnz <$ string "jnz " <*> val <* char ' ' <*> val
  <|> Tgl <$ string "tgl " <*> val

int :: Parser Int
int = read <$> many1 (char '-' <|> digit)

val :: Parser Val
val = V <$> int <|> R <$> reg

reg :: Parser Reg
reg = oneOf "abcd"

readInstrs :: String -> [Instr]
readInstrs = map readInstr . lines
  where
    readInstr s = case runParser instruction () "" s of
      Left err -> error (show err)
      Right i -> i

initMap = M.fromList ([(r,0)|r <- "abcd"])

type Prog = Array Int Instr

readProg :: String -> Prog
readProg s = listArray (0, length is - 1) is
  where
    is = readInstrs s

exec :: Prog -> M.Map Char Int -> Int
exec p = go 0 p
  where
    maxPC = snd $ bounds p
    go pc p m
      | pc > maxPC = m M.! 'a'
      | otherwise  = case (p ! pc) of
          Cpy v (R r) -> go (pc+1) p (M.insert r (value m v) m)
          Inc (R r)   -> go (pc+1) p (M.adjust succ r m)
          Dec (R r)   -> go (pc+1) p (M.adjust pred r m)
          Jnz v1 v2   -> case (value m v1 /= 0) of
                           True  -> go (pc + value m v2) p m
                           False -> go (pc+1) p m
          Tgl v       -> traceShow (pc,p,m) $ go (pc+1) (p & ix (pc + value m v) %~ toggle) m
          _           -> go (pc+1) p m
    value _ (V v) = v
    value m (R r) = fromJust $ M.lookup r m

    toggle :: Instr -> Instr
    toggle (Inc v) = Dec v
    toggle (Tgl v) = Inc v
    toggle (Dec v) = Inc v      -- all other 1-arg
    toggle (Jnz x y) = Cpy x y
    toggle (Cpy x y) = Jnz x y  -- all other 2-arg

main = do
  p <- readProg <$> getContents
  print $ exec p (M.insert 'a' 7 initMap)
  -- print $ exec p (M.insert 'a' 12 initMap)
  print (product [1..12] + 97*80)

