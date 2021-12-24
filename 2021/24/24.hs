-- I ultimately solved day 24 by hand, but used the code in this
-- module to help me explore!

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

import           Control.Applicative        (many, (<|>))
import           Control.Arrow              ((>>>))
import           Control.Lens
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State        (State, execState)
import           Data.Data                  (Data)
import           Data.Data.Lens             (uniplate)
import           Data.List                  (scanl')
import           Data.Map                   (Map, (!))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Text.Megaparsec            (Parsec, parse)
import           Text.Megaparsec.Char       (space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

------------------------------------------------------------
-- AST

data Register = W | X | Y | Z deriving (Eq, Ord, Show, Read, Bounded, Enum)
data RVal = Reg Register | Val Integer deriving (Eq, Ord, Show)
data Instruction = Inp Register | Bin Op Register RVal deriving (Eq, Ord, Show)
data Op = Add | Mul | Div | Mod | Eql deriving (Eq, Ord, Show, Generic, Data)

------------------------------------------------------------
-- Parser

type Parser = Parsec Void String

readParser p = parse p "" >>> either undefined id

parseProgram :: Parser [Instruction]
parseProgram = many (parseInstruction <* space)

parseInstruction :: Parser Instruction
parseInstruction =
      Inp <$> (string "inp" *> space *> parseReg)
  <|> Bin <$> parseOp <*> (space *> parseReg) <*> (space *> parseRVal)

parseOp :: Parser Op
parseOp =
      Add <$ string "add"
  <|> Mul <$ string "mul"
  <|> Div <$ string "div"
  <|> Mod <$ string "mod"
  <|> Eql <$ string "eql"

parseReg :: Parser Register
parseReg =
      W <$ string "w"
  <|> X <$ string "x"
  <|> Y <$ string "y"
  <|> Z <$ string "z"

parseRVal :: Parser RVal
parseRVal = Reg <$> parseReg <|> Val <$> signed space decimal

------------------------------------------------------------
-- Interpretation

data ALU v = ALU { _inputs :: [v], _registers :: Map Register v }
  deriving (Eq, Show)

makeLenses ''ALU

type ALUM v = ReaderT (Semantics v) (State (ALU v))

data Semantics v = Semantics
  { litSem   :: Integer -> v
  , inputSem :: Register -> ALUM v ()
  , binSem   :: Op -> Register -> RVal -> ALUM v ()
  }

runALU :: Semantics v -> [v] -> [Instruction] -> ALU v
runALU sem inp is =
  execState (runReaderT (exec is) sem) (ALU inp (M.fromList (map (,zero) [W .. Z])))
  where
    zero = litSem sem 0

exec :: [Instruction] -> ALUM v ()
exec = mapM_ execOne

execOne :: Instruction -> ALUM v ()
execOne (Inp r)      = ask >>= \sem -> inputSem sem r
execOne (Bin op l r) = ask >>= \sem -> binSem sem op l r

getInput :: ALUM v v
getInput = do
  is <- use inputs
  inputs .= tail is
  return (head is)

input :: Register -> ALUM v ()
input r = do
  v <- getInput
  registers . ix r .= v

bin :: (Op -> v -> v -> v) -> Op -> Register -> RVal -> ALUM v ()
bin eval op l r = do
  v1 <- fromJust <$> use (registers . at l)
  v2 <- evalRVal r
  let v = eval op v1 v2
  registers . ix l .= v

evalRVal :: RVal -> ALUM v v
evalRVal (Reg r) = fromJust <$> use (registers . at r)
evalRVal (Val v) = ask >>= \sem -> return (litSem sem v)

--------------------------------------------------
-- Normal integer semantics

evalOp :: Op -> Integer -> Integer -> Integer
evalOp = \case
  Add -> (+)
  Mul -> (*)
  Div -> div
  Mod -> mod
  Eql -> (\x y -> if x == y then 1 else 0)

intSem :: Semantics Integer
intSem = Semantics id input (bin evalOp)

runALUInt :: Integer -> [Instruction] -> ALU Integer
runALUInt inp = runALU intSem (digits inp)

digits = reverse . explode
  where
    explode 0 = []
    explode n = n `mod` 10 : explode (n `div` 10)

--------------------------------------------------
-- Algebraic semantics

data Expr = ELit Integer | EInput Int | EBin Op Expr Expr
  deriving (Eq, Ord, Show, Generic, Data)

instance Plated Expr where
  plate = uniplate

exprSem :: Semantics Expr
exprSem = Semantics ELit input (bin EBin)

runALUExpr :: [Instruction] -> Map Register Expr
runALUExpr = runALU exprSem (map EInput [0..]) >>> view registers >>> fmap simplify

simplify :: Expr -> Expr
simplify = transform simple
  where
    simple :: Expr -> Expr
    simple (EBin Add (ELit 0) x)       = x
    simple (EBin Add x (ELit 0))       = x
    simple (EBin Mul (ELit 0) _)       = ELit 0
    simple (EBin Mul _ (ELit 0))       = ELit 0
    simple (EBin Mul (ELit 1) x)       = x
    simple (EBin Mul x (ELit 1))       = x
    simple (EBin op (ELit x) (ELit y)) = ELit (evalOp op x y)

    simple e                           = e

------------------------------------------------------------

pattern TheLoop a b c = [Inp W,Bin Mul X (Val 0),Bin Add X (Reg Z),Bin Mod X (Val 26),Bin Div Z (Val a),Bin Add X (Val b),Bin Eql X (Reg W),Bin Eql X (Val 0),Bin Mul Y (Val 0),Bin Add Y (Val 25),Bin Mul Y (Reg X),Bin Add Y (Val 1),Bin Mul Z (Reg Y),Bin Mul Y (Val 0),Bin Add Y (Reg W),Bin Add Y (Val c),Bin Mul Y (Reg X),Bin Add Z (Reg Y)]

match :: [Instruction] -> Maybe (Integer, Integer, Integer)
match (TheLoop a b c) = Just (a,b,c)
match _=              Nothing

vs :: [(Integer,Integer,Integer)]
vs = [(1,11,8),(1,14,13),(1,10,2),(26,0,7),(1,12,11),(1,12,4),(1,12,13),(26,-8,13),(26,-9,10),(1,11,1),(26,0,2),(26,-5,14),(26,-6,6),(26,-12,14)]

gen :: (Integer,Integer,Integer) -> [Instruction]
gen (a,b,c) = TheLoop a b c

type Sim a = (Integer,Integer,Integer) -> Integer -> (a -> a)

sim :: Sim Integer
sim (a,b,c) inp z
  | z `mod` 26 + b == inp = z'
  | otherwise = 26*z' + inp + c
  where
    z' = z `div` a

sim2 :: Sim [Integer]
sim2 (a,b,c) inp []     = if inp == b then [] else [inp+c]
sim2 (a,b,c) inp (z:zs) = if z + b == inp then zs' else (inp+c):zs'
  where
    zs' | a == 26 = zs
        | otherwise = z:zs

runSim :: Sim a -> a -> [(Integer, Integer, Integer)] -> Integer -> [a]
runSim s zero steps input = scanl' (\z (step,i) -> s step i z) zero (zip steps (digits input))

ok :: Integer -> Bool
ok inp = Just (last (runSim sim 0 vs inp)) == (runALUInt inp (concatMap gen vs) ^. registers . at Z)

ok2 :: Integer -> Bool
ok2 inp = Just (foldr (\d n -> d + 26*n) 0 (last (runSim sim2 [] vs inp))) ==
  (runALUInt inp (concatMap gen vs) ^. registers . at Z)
