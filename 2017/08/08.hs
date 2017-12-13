{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Monad.State
import qualified Data.Map               as M
import           Data.Maybe
import           Text.Parsec            hiding (State)
import           Text.Parsec.Char
import           Text.Parsec.String

type Reg = String
data Act = Inc | Dec
  deriving (Eq, Ord, Show, Read)
data Cond = Cond Reg Cmp Int
type Cmp = Int -> Int -> Bool
data Instr = Instr Reg Act Int Cond

parseInstr :: Parser Instr
parseInstr = Instr <$> parseReg
                   <*> (char ' ' *> parseAct)
                   <*> (char ' ' *> int)
                   <*> (char ' ' *> parseCond)

parseReg = many letter

parseAct = Inc <$ string "inc" <|> Dec <$ string "dec"

parseCond = Cond <$> (string "if " *> parseReg) <*> (char ' ' *> parseCmp) <*> (char ' ' *> int)

parseCmp =
      (==) <$ string "=="
  <|> (/=) <$ string "!="
  <|> (>=) <$ try (string ">=")
  <|> (<=) <$ try (string "<=")
  <|> (>) <$ string ">"
  <|> (<) <$ string "<"

readInstr :: String -> Instr
readInstr = readParser parseInstr

readParser :: Parser a -> String -> a
readParser p s = case runParser p () "" s of
  Left err -> error (show err)
  Right a  -> a

int :: Parser Int
int = read <$> many1 (char '-' <|> digit)

type Mem = M.Map Reg Int

look r m = fromMaybe 0 $ M.lookup r m

execI :: Instr -> Mem -> Mem
execI (Instr r act by cond) m
  | evalCond m cond = alterReg r act by m
  | otherwise       = m

evalCond m (Cond r cmp i) = cmp (look r m) i

alterReg r act by m = M.alter f r m
  where
    f Nothing = Just $ doAct act 0 by
    f (Just x) = Just $ doAct act x by
    doAct Inc = (+)
    doAct Dec = (-)

execP :: [Instr] -> Mem -> Mem
execP [] m = m
execP (i:is) m = execP is (execI i m)

traceP :: [Instr] -> Mem -> [Mem]
traceP [] m = [m]
traceP (i:is) m = m : traceP is (execI i m)

main = do
  prog <- (map readInstr . lines) <$> getContents

  -- Part 1
  let m = execP prog M.empty
  print (maximum $ M.elems m)

  -- Part 2
  let ms = traceP prog M.empty
  print (maximum . concat . map M.elems $ ms)
