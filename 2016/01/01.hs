{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Char
import qualified Data.Set            as S
import           Text.Printf

data Turn = L | R
  deriving (Read, Show)
data Instr = Instr Turn Int

type Loc = (Int,Int)
type Dir = (Int,Int)

data Pos = Pos
  { _dir         :: Dir
  , _loc         :: Loc
  , _visited     :: S.Set Loc
  , _firstRepeat :: Maybe Loc
  }

initPos :: Pos
initPos = Pos (0,1) (0,0) S.empty Nothing

makeLenses ''Pos


main = interact solve

solve = showResult . interpret . map parseCommand . words

showResult :: Pos -> String
showResult p = unlines $
  [ printf "Total distance: %d" (distance (p ^. loc))
  , printf "First repeat: %s"   (show (p ^. firstRepeat))
  , printf "Distance to repeat: %d" (maybe 0 distance (p ^. firstRepeat))
  ]

interpret :: [Instr] -> Pos
interpret instrs = execState (interpretS instrs) initPos
  where
    interpretS :: [Instr] -> State Pos ()
    interpretS = mapM_ interpInstr
    interpInstr (Instr t n) = turn t >> replicateM n move
    turn L = dir %= \(x,y) -> (-y,x)
    turn R = dir %= \(x,y) -> (y,-x)
    move   = do
      (dx,dy) <- use dir
      loc %= \(x,y) -> (x+dx, y+dy)
      newLoc <- use loc
      oldLocs <- use visited
      when (S.member newLoc oldLocs) $ firstRepeat %= (<|> Just newLoc)
      visited %= S.insert newLoc

parseCommand :: String -> Instr
parseCommand (d:n) = Instr (read [toUpper d]) (read (takeWhile isDigit n))

distance :: Loc -> Int
distance (x,y) = abs x + abs y
