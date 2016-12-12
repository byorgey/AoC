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

type Reg = Char
data Val = V Int | R Reg deriving Show
data Instr
  = Cpy Val Reg
  | Inc Reg
  | Dec Reg
  | Jnz Val Val
  deriving Show

instruction :: Parser Instr
instruction =
      Cpy <$ string "cpy " <*> val <* char ' ' <*> reg
  <|> Inc <$ string "inc " <*> reg
  <|> Dec <$ string "dec " <*> reg
  <|> Jnz <$ string "jnz " <*> val <* char ' ' <*> val

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

----------------------------------------------------------------------
-- First solution (overengineered)
----------------------------------------------------------------------

type M = ExceptT () (WriterT [String] (State ExecState))

data Zipper a = Z [a] a [a]
  deriving Show

focus :: Zipper a -> a
focus (Z _ a _) = a

left, right :: Zipper a -> M (Zipper a)
left  (Z (l:ls) a r) = return $ Z ls l (a:r)
right (Z _ _ [])     = throwError ()
right (Z l a (r:rs)) = return $ Z (a:l) r rs

data ExecState = ES
  { _prog :: Zipper Instr
  , _mem  :: M.Map Reg Int
  }
  deriving Show

makeLenses ''ExecState

initMap = M.fromList ([(r,0)|r <- "abcd"])

initState :: [Instr] -> ExecState
initState (i:is) = ES (Z [] i is) initMap

interp :: [Instr] -> ([String], ExecState) -- ([String], M.Map Reg Int)
interp is = (_1 %~ snd) $ runState (runWriterT (runExceptT (forever interp))) (initState is)
  where
    interp :: M ()
    interp = do
      i <- focus <$> use prog
      tell [show i]
      case i of
        Cpy v r   -> value v >>= \v' -> mem . at r .= Just v'
        Inc r     -> mem . at r . _Just %= succ
        Dec r     -> mem . at r . _Just %= pred
        Jnz v1 v2 -> value v1 >>= \v1' -> value v2 >>= \v2' -> when (v1' /= 0) (move (v2'-1))
      move 1
    value :: Val -> M Int
    value (V v) = return v
    value (R r) = fromJust <$> use (mem . at r)

    move :: Int -> M ()
    move 0 = return ()
    move n
      | n > 0 = replicateM_ n $ do
                  p  <- use prog
                  p' <- right p
                  prog .= p'
      | n < 0 = replicateM_ (-n) $ do
                  p  <- use prog
                  p' <- left p
                  prog .= p'

----------------------------------------------------------------------
-- The above works but it was way too overengineered.  I should have
-- instead done something like the following, which is a lot less code
-- and a lot faster to boot.
----------------------------------------------------------------------

type Prog = Array Int Instr

readProg :: String -> Prog
readProg s = listArray (0, length is - 1) is
  where
    is = readInstrs s

exec :: Prog -> M.Map Char Int -> Int
exec p = go 0
  where
    maxPC = snd $ bounds p
    go pc m
      | pc > maxPC = m M.! 'a'
      | otherwise  = case (p ! pc) of
          Cpy v r   -> go (pc+1) (M.insert r (value m v) m)
          Inc r     -> go (pc+1) (M.adjust succ r m)
          Dec r     -> go (pc+1) (M.adjust pred r m)
          Jnz v1 v2 -> case (value m v1 /= 0) of
                         True  -> go (pc + value m v2) m
                         False -> go (pc+1) m
    value _ (V v) = v
    value m (R r) = fromJust $ M.lookup r m

main = do
  p <- readProg <$> getContents

  -- solution 1
  --  print $ interp p
  --  print . (M.! 'a') . view mem . snd . interp $ p

  -- solution 2
  print $ exec p initMap
  print $ exec p (M.insert 'c' 1 initMap)
