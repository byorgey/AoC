{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow
import           Text.Parsec          hiding (State)
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as T

main = interact $
  lines >>> applyAll [solveA,solveB] >>> map show >>> unlines

------------------------------------------------------------

t      = T.makeTokenParser emptyDef
int    = T.integer t
parens = T.parens t
sym    = T.symbol t

expr :: [[String]] -> Parser Integer
expr ops = e
  where
    e       = buildExpressionParser opTable term
    opTable = map (map (\op -> Infix (evalOp op <$ sym op) AssocLeft)) ops
    term    = int <|> parens e

    evalOp "+" = (+)
    evalOp "*" = (*)

------------------------------------------------------------

solveA, solveB :: [String] -> Integer
solveA = solveWith [["+","*"]]
solveB = solveWith [["+"], ["*"]]

solveWith ops = map (readParser (expr ops)) >>> sum

------------------------------------------------------------

applyAll :: [a -> b] -> a -> [b]
applyAll fs a = map ($ a) fs

readParser p = parse p "" >>> either undefined id
