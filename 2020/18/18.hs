{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow
import           Text.Parsec          hiding (State)
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as T

main = interact $
  readInput >>> applyAll [solveA,solveB] >>> map show >>> unlines

type Input = [String]

readInput :: String -> Input
readInput = lines

------------------------------------------------------------

data Expr = Lit Integer | Bin Op Expr Expr deriving Show
data Op = Add | Mul deriving Show

t      = T.makeTokenParser emptyDef
int    = T.integer t
parens = T.parens t
sym    = T.symbol t

expr :: [[Op]] -> Parser Expr
expr ops = e
  where
    e = buildExpressionParser opTable term
    opTable = map (map (\op -> Infix (Bin <$> (op <$ sym (opSym op))) AssocLeft)) ops
    term = Lit <$> int <|> parens e

    opSym Add = "+"
    opSym Mul = "*"

type Output = Integer

------------------------------------------------------------

solveA, solveB :: Input -> Output
solveA = solveWith [[Add,Mul]]
solveB = solveWith [[Add], [Mul]]

solveWith ops = map (readParser (expr ops) >>> eval) >>> sum

eval :: Expr -> Integer
eval (Lit n)      = n
eval (Bin op x y) = evalOp op (eval x) (eval y)

evalOp Add = (+)
evalOp Mul = (*)

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

readParser p = parse p "" >>> either undefined id
