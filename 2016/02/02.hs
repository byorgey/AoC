{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import qualified Data.Set             as S
import           Text.Printf

data Dir = L | R | U | D
  deriving (Read, Show)

type KPLoc = (Int,Int)

-- For part 1
-- validKPLoc :: KPLoc -> Bool
-- validKPLoc (x,y) = x `elem` [0..2] && y `elem` [0..2]

-- Part 2
validKPLoc :: KPLoc -> Bool
validKPLoc (x,y) = (abs x + abs y) <= 2

interpDir :: Dir -> (Int,Int)
interpDir L = (-1,0)
interpDir R = (1,0)
interpDir U = (0,-1)
interpDir D = (0,1)

addLoc :: (Int,Int) -> KPLoc -> KPLoc
addLoc (dx,dy) (x,y)
  | validKPLoc newLoc = newLoc
  | otherwise         = (x,y)
  where
    newLoc = (x+dx, y+dy)

-- Part 1
-- initKPLoc :: KPLoc
-- initKPLoc = (1,1)

-- Part 2
initKPLoc :: KPLoc
initKPLoc = (-2,0)

-- Part 1
-- showKP :: KPLoc -> String
-- showKP (x,y) = show (3*y + x + 1)

-- Part 2
showKP :: KPLoc -> String
showKP (_,-2) = "1"
showKP (x,-1) = ["234" !! (x+1)]
showKP (x,0)  = ["56789" !! (x+2)]
showKP (x,1)  = ["ABC" !! (x+1)]
showKP (_,2)  = "D"

main = interact solve

solve = show . execute . lines

execute :: [String] -> String
execute ls = execWriter (evalStateT (interp ls) initKPLoc)
  where
    interp :: [String] -> StateT KPLoc (Writer String) ()
    interp = mapM_ interpOne
    interpOne :: String -> StateT KPLoc (Writer String) ()
    interpOne s = do
      mapM_ interpChar s
      s <- gets showKP
      tell s
    interpChar c = modify (addLoc (interpDir (read [c])))
