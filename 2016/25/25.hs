{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Array
import qualified Data.Map             as M
import           Data.Maybe
import           System.Environment
import           Text.Parsec          hiding (State)
import           Text.Parsec.String

type Reg = Char
data Val = V Int | R Reg deriving Show
data Instr
  = Cpy Val Reg
  | Inc Reg
  | Dec Reg
  | Jnz Val Val
  | Out Val
  deriving Show

instruction :: Parser Instr
instruction =
      Cpy <$ string "cpy " <*> val <* char ' ' <*> reg
  <|> Inc <$ string "inc " <*> reg
  <|> Dec <$ string "dec " <*> reg
  <|> Jnz <$ string "jnz " <*> val <* char ' ' <*> val
  <|> Out <$ string "out " <*> val

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

exec :: Prog -> M.Map Char Int -> [Int]
exec p = go 0
  where
    maxPC = snd $ bounds p
    go pc m
      | pc > maxPC = []
      | otherwise  = case (p ! pc) of
          Cpy v r   -> go (pc+1) (M.insert r (value m v) m)
          Inc r     -> go (pc+1) (M.adjust succ r m)
          Dec r     -> go (pc+1) (M.adjust pred r m)
          Jnz v1 v2 -> case (value m v1 /= 0) of
                         True  -> go (pc + value m v2) m
                         False -> go (pc+1) m
          Out v     -> value m v : go (pc+1) m
    value _ (V v) = v
    value m (R r) = fromJust $ M.lookup r m

main = do
  [a] <- getArgs
  p <- readProg <$> getContents

  print $ exec p (M.insert 'a' (read a) initMap)

