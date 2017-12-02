{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map            as M
import           Text.Parsec         hiding (State)
import           Text.Parsec.String

data Target
  = TBot Int
  | TOutput Int
  deriving Show

data Instr
  = Value Int Target
  | Comp Int Target Target
  deriving Show

data Bot = Bot
  { _botVals :: [Int]
  , _botLo   :: Target
  , _botHi   :: Target
  }
  deriving Show

data World = World
  { _bots    :: M.Map Int Bot
  , _outputs :: M.Map Int [Int]
  , _answer  :: Maybe Int
  }
  deriving Show

makeLenses ''World
makeLenses ''Bot

initWorld :: World
initWorld = World M.empty M.empty Nothing

int :: Parser Int
int = read <$> many1 digit

parseInstr :: Parser Instr
parseInstr =
  Value <$  string "value "
        <*> int
        <*  string " goes to "
        <*> parseTarget
  <|>
  Comp  <$  string "bot "
        <*> int
        <*  string " gives low to "
        <*> parseTarget
        <*  string " and high to "
        <*> parseTarget

parseTarget :: Parser Target
parseTarget =
      TBot <$ string "bot " <*> int
  <|> TOutput <$ string "output " <*> int

readInstr :: String -> Instr
readInstr s = case runParser parseInstr () "" s of
  Left err -> error (show err)
  Right i -> i

runWorld :: [Instr] -> World
runWorld p = execState (runWorldSt p) initWorld
  where
    runWorldSt :: [Instr] -> State World ()
    runWorldSt instrs = setupWorld instrs >> execWorld

    setupWorld instrs = do
      let botIs = [(c,t1,t2) | Comp c t1 t2 <- instrs]
          vals = [(v,t) | Value v t <- instrs]
      mapM_ mkBot botIs
      mapM_ giveValue vals

    mkBot (c, t1, t2) = bots . at c .= Just (Bot [] t1 t2)
    giveValue (v, TBot b)    = bots . at b . _Just . botVals %= (v:)
    giveValue (v, TOutput o) = outputs %= M.alter (maybe (Just [v]) (Just . (v:))) o

    execWorld :: State World ()
    execWorld = (M.keys <$> use bots) >>= mapM_ processBot
      where
        processBot :: Int -> State World ()
        processBot b = do
          Just bot <- M.lookup b <$> use bots
          case bot of
            Bot [v1,v2] tLo tHi -> do
              when ([17,61] == sort [v1,v2]) $ answer .= Just b
              bots . at b . _Just . botVals .= []
              giveValue (min v1 v2, tLo)
              giveValue (max v1 v2, tHi)
              maybeRecurse tLo
              maybeRecurse tHi
            _ -> return ()
        maybeRecurse (TBot b) = processBot b
        maybeRecurse _        = return ()

main = interact (show . runWorld . map readInstr . lines)
